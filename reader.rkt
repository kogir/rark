#lang racket

(provide (rename-out [arc-make-readtable make-readtable]
                     [arc-read read]
                     [arc-read-syntax read-syntax])
         read-wrapper 
         set-compiler-debug!)

;;;
;;; Debugging Helpers
;;;

(define compiler-debug #f)

(define (set-compiler-debug! flag)
  (set! compiler-debug (eq? flag #t)))

;;;
;;; Helper syntax
;;;

(define-syntax-rule (datum->syntax/clone src-stx datum)
  (datum->syntax src-stx datum src-stx src-stx))

(define-syntax-rule (with-arc-readtable expr ...)
  (parameterize ([current-readtable (arc-make-readtable)])
    expr ...))

;;;
;;; Readtable Extensions:
;;; =====================
;;; [ stuff ... ] => (square-brackets stuff ...)
;;; This allows nesting and extension from arc itself.
;;;

(define (read-square-brackets ch port src line col pos)
  (let ([next (read-syntax/recursive src port #\[ #f)])
    (datum->syntax/clone
     next
     ;`(square-brackets ,next))))
     `(fn (_) ,next))))

(define (arc-make-readtable)
  ; Only [ _ ] lambdas can be handled at the readtable level.
  ; Other language forms are implemented as macros, special
  ; (%#top, %#app, etc) forms, or in the reader.
  (make-readtable #f
                  #\[ 'terminating-macro read-square-brackets))

;;
;; Reader Extensions
;;

(define (read-wrapper thunk read-syntax)
  (with-arc-readtable (thunk)))

(define (arc-read-syntax src in)
  (let* ([next (read-syntax src in)]
         [expanded (ssyntax-expand next)])
    (when (and compiler-debug
               (not (eof-object? next)))
      (printf "Source: ~s\n" next)
      (printf "Expanded: ~s\n" expanded))
    (or expanded next)))

(define (arc-read in)
  (let ([syntax (arc-read-syntax 'none in)])
    (if (eof-object? syntax)
        syntax
        (syntax->datum syntax))))

;;;
;;; SSyntax Expansion:
;;; ==================
;;; ssyntax-expand-* functions should return #f or an expanded syntax object.
;;; (a&b ...) => (and-ssyntax (a b) ...)
;;;

(define (ssyntax-expand stx)
  (and (syntax? stx)
       (or (ssyntax-expand-list stx)
           (and (ssyntax? stx)
                (perform-ssyntax-expand stx)))))

(define (ssyntax-expand-list stx)
  (let ([stx-list (syntax->list stx)])
    (and stx-list
         (or (ssyntax-expand-proc stx-list)
             (ssyntax-expand-preserve stx stx-list)))))

(define (ssyntax-expand-proc stx-list)
  (and (not (null? stx-list))
       (ssyntax? (car stx-list))
       (perform-ssyntax-expand (car stx-list) (cdr stx-list))))

(define (ssyntax-expand-preserve stx-ctx stx-list)
  (let ([expanded-list (map ssyntax-expand stx-list)])
    (and (ormap identity expanded-list)
         (datum->syntax/clone
          stx-ctx
          (map (lambda (old new) (or new old))
               stx-list
               expanded-list)))))

(define (perform-ssyntax-expand stx [stx-arg-list null])
  (let* ([sym (syntax-e stx)]
         [string-id (symbol->string sym)]
         [datum (cond [(ssyntax-compose? string-id)
                       (ssyntax-expand-compose string-id stx stx-arg-list)]
                      [(and (ssyntax-sexpr? string-id)
                            (null? stx-arg-list))
                       ; Only expand sexprs as expressions, never procedures
                       (ssyntax-expand-sexpr sym)]
                      [(ssyntax-and? string-id)
                       (ssyntax-expand-and string-id stx stx-arg-list)]
                      [else #f])]
         [result (and datum
                      (datum->syntax/clone stx datum))])
    (or (ssyntax-expand result) result)))


(define (ssyntax-expand-sexpr sym)
  (build-sexpr (reverse (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
               sym))

(define (ssyntax-expand-compose string-id stx-ctx stx-args)
  (define (ssyntax-expand-complement string-id)
    ;(regexp-replace* #rx"~(?!($|:))" string-id "complement-ssyntax:"))
    (regexp-replace* #rx"~(?!($|:))" string-id "no:"))
  
  (define (compose-call funs args)
    (if (null? (cdr funs))
        (if (list? args)
            `(,(car funs) ,@args)
            `(apply ,(car funs) ,args))            
        `(,(car funs) ,(compose-call (cdr funs) args))))
  
  (let* ([negated (ssyntax-expand-complement string-id)]
         [parts (regexp-split #rx"(?<!(^|:)):(?!($|:))" negated)]
         [symbols (map string->symbol parts)]
         [funcs   (map (lambda (sym) (datum->syntax/clone stx-ctx sym))
                       symbols)])
    (if (null? stx-args)
        `(fn args ,(compose-call funcs 'args)) ; Expression
        (compose-call funcs stx-args))))

(define (ssyntax-expand-and string-id stx-ctx stx-args)
  (let* ([parts   (regexp-split #rx"(?<!(^|&))&(?!($|&))" string-id)]
         [symbols (map string->symbol parts)]
         [funcs   (map (lambda (sym) (datum->syntax/clone stx-ctx sym))
                       symbols)]
         [expanded-funcs (or (ssyntax-expand-preserve stx-ctx funcs) funcs)]
         [expanded-args  (or (ssyntax-expand-preserve stx-ctx stx-args) stx-args)])
    `(and-ssyntax ,expanded-funcs ,@expanded-args)))

(define (build-sexpr toks orig)
  (cond ((null? toks)
         'get)
        ((null? (cdr toks))
         (chars->value (car toks)))
        (#t
         (list (build-sexpr (cddr toks) orig)
               (if (eqv? (cadr toks) #\!)
                   (list 'quote (chars->value (car toks)))
                   (if (or (eqv? (car toks) #\.) (eqv? (car toks) #\!))
                       (error "Bad ssyntax" orig)
                       (chars->value (car toks))))))))

(define (ssyntax? stx)
  (and (identifier? stx)
       (let ([string-id (symbol->string (syntax-e stx))])
         (or (ssyntax-compose? string-id)
             (ssyntax-sexpr? string-id)
             (ssyntax-and? string-id)))))

(define (ssyntax-compose? string-id)
  (or (regexp-match? #rx"~(?!($|:))" string-id)
      (regexp-match? #rx"(?<!(^|:)):(?!($|:))" string-id)))

(define (ssyntax-sexpr? string-id)
  ; Had to tighten this up to allow use of ... in macros
  ; In short, these mean ! or . neither preceeded nor followed by . or !
  (or (regexp-match? #rx"(?<!(^|[!.]))[.](?!($|[!.]))" string-id)
      (regexp-match? #rx"(?<![!.])[!](?!($|[!.]))" string-id)))

(define (ssyntax-and? string-id)
  (regexp-match? #rx"(?<!(^|&))&(?!($|&))" string-id))

(define (tokens test source token acc keepsep?)
  (cond ((null? source)
         (reverse (if (pair? token)
                      (cons (reverse token) acc)
                      acc)))
        ((test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (let ((rec (if (null? token)
                                acc
                                (cons (reverse token) acc))))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (#t
         (tokens test
                 (cdr source)
                 (cons (car source) token)
                 acc
                 keepsep?))))

(define (symbol->chars x) (string->list (symbol->string x)))

(define (chars->value chars) (read-from-string (list->string chars)))

(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

