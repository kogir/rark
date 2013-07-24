#lang racket

;;;
;;; Arc Runtime
;;;

(require (for-syntax syntax/parse
                     (file "shared-syntax.rkt"))
         (prefix-in arc- (file "../reader.rkt"))
         (file "core.rkt")
         (file "shared-syntax.rkt")
         ffi/unsafe
         racket/provide
         racket/splicing
         openssl/sha1)

(provide (matching-identifiers-out #rx"^arc-" (all-defined-out)))

(define-syntax (xdef stx)
  (define-splicing-syntax-class id-pair
    #:description "arc mapping pair"
    #:attributes (arc-name racket-name)
    (pattern (~seq arc-name:id)
             #:with racket-name #'arc-name)
    (pattern [arc-name:id racket-name:id]))
  
  (syntax-parse
   stx
   [(_ m:id-pair ...+)
    #'(provide (prefix-out arc- (rename-out [m.racket-name m.arc-name] ...)))]))

(xdef ...
      #%datum
      [#%module-begin #%plain-module-begin]
      #%top
      #%top-interaction
      
      compile-allow-set!-undefined
      
      [no core:nil?]
      
      void
      
      all-defined-out
      all-from-out
      file
      for-syntax
      for-template
      only-in
      provide
      planet
      rename-out
      require
      
      begin-for-syntax
      define-for-syntax
      syntax-case
      syntax
      
      apply ; TODO: Is this safe?
      compose
      [do1 begin0]
      [break-thread break-thread]
      ;[ccc call-with-current-continuation]
      [gc collect-garbage]
      [stderr current-error-port]
      [current-gc-milliseconds current-gc-milliseconds]
      [stdin  current-input-port]
      [memory current-memory-use]
      [msec current-milliseconds]
      [current-process-milliseconds current-process-milliseconds]
      [stdout current-output-port]
      [seconds current-seconds]
      [current-thread current-thread]
      [rmfile delete-file]
      [dms dump-memory-stats]
      eval
      [even even?]
      [quit exit]
      [uniq gensym]
      [inside get-output-string]
      [kill-thread kill-thread]
      [newstring make-string]
      [complement negate]
      [odd odd?]
      [infile open-input-file]
      [instring open-input-string]
      istring
      [outstring open-output-string]
      quasiquote
      quote
      [rand random] ; need to use a better seed
      sleep
      system
      [new-thread thread]
      [dead thread-dead?]
      unquote
      unquote-splicing
      
      ; Strings (for speed)
      [rkt-string=? string=?]
      [rkt-string<? string<?]
      [rkt-string<=? string<=?]
      [rkt-string>? string>?]
      [rkt-string>=? string>=?]
      [rkt-substring substring]
      [rkt-string-length string-length]
      
      ; Math
      [sin sin]
      [cos cos]
      [tan tan]
      [asin asin]
      [acos acos]
      [atan atan]
      [log log]
      
      ; Incorrect Racket Exports
      displayln
      printf)

;;;
;;; This is a temporary dirty hack
;;;
(define-syntax (arc-ensure-var stx)
  (syntax-parse
   stx
   [(_ name:id)
    (case (syntax-local-context)
      [(top-level) (if (compile-allow-set!-undefined)
                       #'(set! name (namespace-variable-value 'name #t (lambda () arc-nil)))
                       (raise-syntax-error #f
       "(compile-allow-set!-undefined) must be true to use globals in load mode"
                                           stx))]
      [else (if (lexically-bound? #'name)
                #'(void)
                #'(=2 name arc-nil))])]))

(define arc-permwrite (make-hash))

(define-syntax-rule (perm-check id body)
  (if (hash-has-key? arc-permwrite 'id)
      (begin0 body
              ((hash-ref arc-permwrite 'id) id))
      body))

(define-syntax (set-check-perm stx)
  (syntax-parse
   stx
   [(_ p:binding-pair)
    ;; TODO: Implement perm!
    #'(begin (set! p.var p.rhs) p.var)]))

; This allows cross module mutation of globals
(define-syntax (declare-with-set-transformer stx)
  (syntax-parse
   stx
   [(_ var:id)
    #'(declare-with-set-transformer var (gensym 'unintialized))]
   [(_ var:id init-val:expr)
    (let ([store-name (gensym (syntax->datum #'var))])
      #`(begin
          (define #,store-name init-val)
          (splicing-let ([set (lambda (val)
                                (set! #,store-name val))])
            (define-syntax var
              (make-set!-transformer
               (lambda (stx)
                 (syntax-case stx (set!)
                   [(set! id val) #'(set val)]
                   [(id args (... ...)) #'(arc-#%app #,store-name args (... ...))] ; function application
                   [id (identifier? #'id) #'#,store-name])))))))]))

(define-syntax (assign1-unbound stx)
  (syntax-parse
   stx
   [(_ p:binding-pair)
    (case (syntax-local-context)
      [(top-level) (if (compile-allow-set!-undefined)
                       #'(set-check-perm p.var p.rhs)
                       (raise-syntax-error #f
       "(compile-allow-set!-undefined) must be true to use globals in load mode"
                                           stx))]
      ; In proper modules we require things to be defined
      [(module) #'(declare-with-set-transformer p.var p.rhs)]
      ; In local contexts, check for existing binding and error if not found
      [else #'(if (arc-bound p.var)
                  (set-check-perm p.var p.rhs)
                  (raise-syntax-error
                   'p.var
                   "reference to an identifier before its definition"))])]))

;; TODO: Disallow creating globals accidentally from local scopes.
;; TODO: Does perm work with local aliases?
;; TODO: Disallow t and nil assignment
; Note: Perm bindings are only checked at top-level and module contexts
(define-syntax (assign1 stx)
  (syntax-parse
   stx
   [(_ p:binding-pair)
    (let* ([expanded (local-expand #'p.var (syntax-local-context) #f)]
           [binding (and (identifier? expanded) (identifier-binding expanded))])
      (cond [(eq? binding 'lexical)
             ; Locally bound - No need to check for perm
             #'(begin (set! p.var p.rhs) p.var)]
            [binding
             ; Module bound - check perm, set transformer allows mutation
             #'(set-check-perm p.var p.rhs)]
            [else
             ; Unbound - may exist as (%#top . id)
             #'(assign1-unbound p.var p.rhs)]))]))

(define-syntax (arc-assign stx)
  (syntax-parse
   stx
   [(_ p:binding-pair ...+)
    #'(begin (assign1 p.var p.rhs) ...)]))

;; Note: leftmost argument must be fully expanded!
(define-syntax (=2 stx)
  (define-splicing-syntax-class list-set-form
    #:description "assignment target"
    #:attributes (set-form target)
    (pattern ((~datum car) target:id)
             #:attr set-form #'(lambda (val) (arc-scar target val)))
    (pattern ((~datum cdr) target:id)
             #:attr set-form #'(lambda (val) (arc-scdr target val)))
    (pattern ((~datum caar) target:id)
             #:attr set-form #'(lambda (val) (arc-scar (arc-car target) val)))
    (pattern ((~datum cadr) target:id)
             #:attr set-form #'(lambda (val) (arc-scar (arc-cdr target) val)))
    (pattern ((~datum caar) target:id)
             #:attr set-form #'(lambda (val) (arc-scdr (arc-cdr target) val))))
  
  (syntax-parse
   stx
   ;; TODO Allow extension from arc
   ; For simple identifiers, just assign
   [(_ place:id val:expr)
    #'(assign1 place val)]
   ; For list references more magic is required
   [(_ place:list-set-form val:expr)
    #'(perm-check place.target
                  (let [(tmp val)]
                    (place.set-form tmp)
                    tmp))]
   ; Handle hash tables, list element references, etc.
   [(_ (com:expr indx:expr) val:expr)
    #'(perm-check com
                  (let [(tmp val)]
                    (arc-sref com tmp indx)
                    tmp))]
   ; Hash table references with default values - default ignored
   ; TODO: More rigorous type checking?
   [(_ (com:expr indx:expr default:expr) val:expr)
    #'(perm-check com
                  (let [(tmp val)]
                    (arc-sref com tmp indx)
                    tmp))]))

(define-syntax (arc-= stx)
  (define-splicing-syntax-class partially-expanding-expression-pair
    #:description "expression pair"
    #:attributes (a b a-expanded)
    ; Don't expand syntax transformers
    (pattern (~seq a:id b:expr)
             #:when (set!-transformer?
                     (syntax-local-value #'a (lambda () #f)))
             #:attr a-expanded #'a)
    (pattern (~seq (a:id args ...) b:expr)
             #:when (set!-transformer?
                     (syntax-local-value #'a (lambda () #f)))
             #:attr a-expanded #'(a args ...))
    (pattern (~seq a:expr b:expr)
             #:attr a-expanded (local-expand #'a
                                             (syntax-local-context)
                                             (list #'arc-#%app))))
  (syntax-parse
   stx
   [(_ s:partially-expanding-expression-pair ...+)
    #'(begin (=2 s.a-expanded s.b) ...)]))

(define-syntax (arc-safeset stx)
  (syntax-parse
   stx
   [(_ name:id val:expr
       (~do (warn-on-redefinition #'name)))
    #'(assign1 name val)]))

;; TODO: Use arc car/cdr here?
(define (pairwise pred lst)
  (cond [(null? lst) arc-t]
        [(null? (cdr lst)) arc-t]
        [(core:~nil? (pred (car lst) (cadr lst)))
         (pairwise pred (cdr lst))]
        [else arc-nil]))

(xdef [err error])

(define (arc-+ . args)
  (cond [(null? args) 0]
        [(char-or-string? (car args))
         (apply string-append 
                (map (lambda (a) (arc-coerce a 'string))
                     args))]
        [(list? (core:nil->null (car args)))
         (apply arc-append args)]
        [else (apply + args)]))

(idefine (char-or-string? x) (or (string? x) (char? x)))

(xdef -
      *
      /
      [mod modulo]
      expt
      sqrt)

; generic comparison

(define-syntax (all stx)
  (syntax-parse
   stx
   [(_ pred arg:id ...)
    #'(and (pred arg) ...)]))

(define (arc->2 x y)
  (cond [(all number? x y) (> x y)]
        [(all string? x y) (string>? x y)]
        [(all symbol? x y) (string>? (symbol->string x)
                                     (symbol->string y))]
        [(all char? x y) (char>? x y)]
        [else (> x y)]))

(define (arc-> . args)
  (pairwise arc->2 args))

(define (arc-<2 x y)
  (cond [(all number? x y) (< x y)]
        [(all string? x y) (string<? x y)]
        [(all symbol? x y) (string<? (symbol->string x)
                                     (symbol->string y))]
        [(all char? x y) (char<? x y)]
        [else (< x y)]))

(define (arc-< . args)
  (pairwise arc-<2 args))

(define (arc-len x)
  (cond [(string? x) (string-length x)]
        [(hash? x) (hash-count x)]
        [else (length (core:nil->null x))]))

(idefine (exint? x) (and (integer? x) (exact? x)))

(define (arc-type x)
  (cond [(pair? x)          'cons]
        [(symbol? x)        'sym]
        [(core:nil? x)      'sym]
        [(procedure? x)     'fn]
        [(char? x)          'char]
        [(string? x)        'string]
        [(exint? x)         'int]
        [(number? x)        'num]     ; unsure about this
        [(hash? x)          'table]
        [(output-port? x)   'output]
        [(input-port? x)    'input]
        [(tcp-listener? x)  'socket]
        [(exn? x)           'exception]
        [(thread? x)        'thread]
        [else               (error 'type "unknown type: ~s" x)]))

(define (arc-outfile f [mode #f])
  (open-output-file f 
                    #:'text
                    #:exists (if (eq? mode 'append)
                                 'append
                                 'truncate)))

(define (arc-call-w/stdout port thunk)
  (parameterize ((current-output-port port)) (thunk)))

(define (arc-call-w/stdin port thunk)
  (parameterize ((current-input-port port)) (thunk)))

(define (arc-readc . str)
  (let ([c (read-char (if (pair? str)
                          (car str)
                          (current-input-port)))])
    (if (eof-object? c) arc-nil c)))

(define (arc-readb . str)
  (let ([c (read-byte (if (pair? str)
                          (car str)
                          (current-input-port)))])
    (if (eof-object? c) arc-nil c)))

(define (arc-writec c . args) 
  (write-char c 
              (if (pair? args) 
                  (car args) 
                  (current-output-port)))
  c)

(define (arc-writeb b . args)
  (write-byte b 
              (if (pair? args) 
                  (car args) 
                  (current-output-port)))
  b)

(define explicit-flush #f)

(define (printwith f args)
  (let ([port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))])
    (when (pair? args)
      (f (car args) port))
    (unless explicit-flush (flush-output port))))

(define (arc-write . args) (printwith write   args))
(define (arc-disp  . args) (printwith display args))

; sread = scheme read. eventually replace by writing read

;; TODO: *** make sure this is the correct reader
(define (arc-sread p eof)
  (let ([expr (arc-read p)])
    (if (eof-object? expr) eof expr)))

(idefine (iround x) (inexact->exact (round x)))

; is write that (coerce nil 'string) -> ""

(define (arc-coerce x type . args)
  (cond [(eqv? type (arc-type x)) x]
        [(char? x)      (case type
                          [(int)     (char->integer x)]
                          [(string)  (string x)]
                          [(sym)     (integer->char (string x))]
                          [else      (error 'coerce "Can't coerce ~s to ~s" x type)])]
        [(exint? x)     (case type
                          [(num)     x]
                          [(char)    (integer->char x)]
                          [(string)  (apply number->string x args)]
                          [else      (error 'coerce "Can't coerce ~s to ~s" x type)])]
        [(number? x)    (case type
                          [(int)     (iround x)]
                          [(char)    (integer->char (iround x))]
                          [(string)  (apply number->string x args)]
                          [else      (error 'coerce "Can't coerce ~s to ~s" x type)])]
        [(string? x)    (case type
                          [(sym)     (string->symbol x)]
                          [(cons)    (string->list x)]
                          [(num)     (or (apply string->number x args)
                                         (error 'coerce "Can't coerce ~s to ~s" x type))]
                          [(int)     (let ([n (apply string->number x args)])
                                       (if n 
                                           (iround n)
                                           (error 'coerce "Can't coerce ~s to ~s" x type)))]
                          [else      (error 'coerce "Can't coerce ~s to ~s" x type)])]
        [(pair? x)      (case type
                          [(string)  (apply string-append
                                            (map (lambda (y) (arc-coerce y 'string)) 
                                                 x))]
                          [else      (error 'coerce "Can't coerce ~s to ~s" x type)])]
        [(core:nil? x)  (case type
                          [(string)  ""]
                          [else      (error 'coerce "Can't coerce ~s to ~s" x type)])]
        [(symbol? x)    (case type 
                          [(string)  (symbol->string x)]
                          [else      (error 'coerce "Can't coerce ~s to ~s" x type)])]
        [else             x]))

;; TODO: hard coded IP is a bit of a hack
(define (arc-open-socket num) (tcp-listen num 500 #t "127.0.0.1"))

(define (arc-socket-accept s)
  (parameterize ([current-custodian (make-custodian)])
    (let*-values ([(in out) (tcp-accept s)]
                  [(server-ip client-ip) (tcp-addresses out)]
                  [(in1) (make-limited-input-port in 100000 #t)])
      (associate-custodian (current-custodian) in1 out)
      (list in1 out client-ip))))

(define (arc-on-err errfn f)
  ((call-with-current-continuation 
     (lambda (k) 
       (lambda () 
         (with-handlers ((exn:fail? (lambda (c) 
                                      (k (lambda () (errfn c)))))) 
                        (f)))))))

(define (arc-pipe-from cmd)
  (let ((tf (ar-tmpname)))
    (system (string-append cmd " > " tf))
    (let ((str (open-input-file tf)))
      (system (string-append "rm -f " tf))
      str)))
                   
(define (ar-tmpname)
  (call-with-input-file "/dev/urandom"
    (lambda (rstr)
      (do ((s "/tmp/")
           (c (read-char rstr) (read-char rstr))
           (i 0 (+ i 1)))
          ((>= i 16) s)
        (set! s (string-append s
                               (string
                                 (integer->char
                                   (+ (char->integer #\a)
                                      (modulo
                                        (char->integer (read-char rstr))
                                        26))))))))))

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(define (arc-table . args)
  (let ([h (make-hash)])
    (when (pair? args) ((car args) h))
    h))

; eq-table in temload makes a big difference
(define (arc-eq-table) (make-hasheq))

(idefine (arc-maptable fn table) ; arg is (fn (key value) ...)
  (hash-for-each table fn)
  table)

(define (arc-protect during after)
  (dynamic-wind (lambda () #t) during after))

(define (arc-dir name)
  (map path->string (directory-list name)))

; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(define (arc-file-exists name)
  (if (file-exists? name) name arc-nil))

(define (arc-dir-exists name)
  (if (directory-exists? name) name arc-nil))

(define (arc-make-dir name)
  (make-directory* name))

(define (arc-mvfile old new)
  (rename-file-or-directory old new #t))

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (disp-to-string x)
  (let ([o (open-output-string)])
    (display x o)
    (close-output-port o)
    (get-output-string o)))

(define (arc-details c)
  (disp-to-string (exn-message c)))

; Later may want to have multiple indices.

(define (arc-sref com val ind)
  (cond [(hash? com) (if (core:nil? val)
                         (hash-remove! com ind)
                         (hash-set! com ind val))]
        [(string? com) (string-set! com ind val)]
        [(pair? com)   (nth-set! com ind val)]
        [else (raise-user-error 'sref
                                "Can't set reference ~s ~s ~s"
                                com ind val)]))

(define-syntax-rule (nth-set! lst n val)
  (arc-scar (list-tail lst n) val))

; If you're calling this directly, you probably meant to use arc-bound instead.
(splicing-let ([top-unbound (gensym)])
  (define (top-bound? sym)
    (if (eq? top-unbound
             (namespace-variable-value sym
                                       #t
                                       (lambda () top-unbound)))
        arc-nil
        arc-t)))

;; TODO: Currently checks local lexical scopes - Is that desired?
(define-syntax (arc-bound stx)
  (syntax-case stx ()
    [(_ sym)
     (if (lexically-bound? #'sym)
         #'arc-t
         #'(top-bound? 'sym))]))

(define (arc-trunc x) (inexact->exact (truncate x)))

; bad name

(define (arc-exact x)
  (exint? x))

(define (arc-thread-milliseconds)
  (current-process-milliseconds (current-thread)))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(define (arc-atomic-invoke f)
  (if (thread-cell-ref ar-sema-cell)
      (f)
      (begin
        (thread-cell-set! ar-sema-cell #t)
        (arc-protect
         (lambda ()
           (call-with-semaphore
            ar-the-sema
            (lambda () (f))))
         (lambda ()
           (thread-cell-set! ar-sema-cell #f))))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.

(define (arc-flushout) (flush-output) arc-t)

;; TODO: Is this still true?
;; Verify and remove if not needed

; there are two ways to close a TCP output port.
; (close o) waits for output to drain, then closes UNIX descriptor.
; (force-close o) discards buffered output, then closes UNIX desc.
; web servers need the latter to get rid of connections to
; clients that are not reading data.
; mzscheme close-output-port doesn't work (just raises an error)
; if there is buffered output for a non-responsive socket.
; must use custodian-shutdown-all instead.

(define custodians (make-hash))

(define (associate-custodian c i o)
  (hash-set! custodians i c)
  (hash-set! custodians o c))

; if a port has a custodian, use it to close the port forcefully.
; also get rid of the reference to the custodian.
; sadly doing this to the input port also kills the output port.

(define (try-custodian p)
  (let ([c (hash-ref custodians p #f)])
    (if c
        (begin
          (custodian-shutdown-all c)
          (hash-remove! custodians p)
          #t)
        #f)))

(define (arc-close . args)
  (map (lambda (p)
         (cond [(input-port? p)   (close-input-port p)]
               [(output-port? p)  (close-output-port p)]
               [(tcp-listener? p) (tcp-close p)]
               [else (error "Can't close " p)]))
       args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  (void))

(define (arc-force-close . args)
  (map (lambda (p)
         (when (not (try-custodian p))
             (arc-close p)))
       args)
  (void))

; how many bytes has this thread allocated, cumulatively?
; only works with a modified mzscheme.

(define (arc-thread-alloced)
  (arc-on-err (lambda (e) 0)
          (lambda() (current-memory-use (current-thread)))))

(define (arc-declare key val)
  (let ([flag (core:~nil? val)])
    (case key
      ;; TODO: Support this?
      ;[(atstrings)      (set! atstrings      flag)]
      ; New runtime makes all calls direct :)
      ;[(direct-calls)   (set! direct-calls   flag)]
      [(explicit-flush) (set! explicit-flush flag)]))
    val)

(let ([quiet (putenv "TZ" ":GMT")])
  (void))

(define (gmt-date sec) (seconds->date sec))

(define (arc-timedate . args)
  (let ([d (gmt-date (if (pair? args) (car args) (current-seconds)))])
    (list (date-second d)
          (date-minute d)
          (date-hour d)
          (date-day d)
          (date-month d)
          (date-year d))))

(define (arc-shash s) (sha1 (open-input-string s)))

;; TODO: Find a way to put this back into arc.arc

(define arc-templates* (make-hash))

(define-syntax (arc-deftem stx)
  (syntax-parse
   stx
   [(_ decl:template-declaration field:template-field ...)
    #`(hash-set! arc-templates*
                 'decl.name
                 (append #,(if (attribute decl.include)
                               #'(filter-map (lambda (x)
                                               (hash-ref arc-templates* x #f))
                                             (reverse '(decl.include ...)))
                               #''())
                         (list (list 'field.name (lambda () field.value)) ...)))]))

(define-syntax (arc-addtem stx)
  (syntax-parse
   stx
   [(_ name:id field:template-field ...)
    #'(hash-set! arc-templates*
                 'name
                 (remove-duplicates
                  (append '((field.name (lambda () field.value)) ...)
                          (hash-ref arc-templates* 'name '()))
                  (lambda (x y) (arc-is (car x) (car y)))))]))

(define-syntax (arc-obj stx)
  (define-splicing-syntax-class obj-field
    #:description "object field entry"
    (pattern (~seq key:expr val:expr)))
  (syntax-parse
   stx
   [(_ field:obj-field ...+)
    #'(let ([h (arc-table)])
        (hash-set! h 'field.key field.val) ...
        h)]))

(idefine (arc-map1 fn seq)
  (map fn (core:nil->null seq)))

(idefine (arc-each1 fn seq)
  (for-each fn (core:nil->null seq)))
