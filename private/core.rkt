#lang racket/base

;;;
;;; Minimal Arc
;;;
;;; This file contains the immutable core upon which the rest of Arc is built.
;;; Changes to anything in this file would fundamentally change Arc itself.
;;; Everything included herein is meant to be inlined, optimized, and whenever
;;; possible, erased by later macro transformation and compilation steps.
;;;

(require (for-syntax racket/base
                     racket/list
                     syntax/parse
                     (file "shared-syntax.rkt"))
         ;racket/match
         racket/provide
         racket/splicing
         racket/stxparam
         racket/unsafe/ops
         (file "shared-syntax.rkt"))

(provide (matching-identifiers-out #rx"^arc-" (all-defined-out))
         ;(rename-out [#%app arc-#%app])
         (prefix-out core: (combine-out nil?
                                        ~nil?
                                        nil->null
                                        nil->#f)))

;;;
;;; Procedure Application
;;;

(define app-apply
  (case-lambda
    [(efn arg)
     (cond [(procedure? efn) (#%app efn arg)]
           [(pair? efn) (#%app list-ref efn arg)]
           [(string? efn) (#%app string-ref efn arg)]
           [(hash? efn) (#%app hash-ref efn arg arc-nil)]
           [else (#%app efn arg)])]
    [(efn arg1 arg2)
     (if (hash? efn)
         ; Have to use the lamda in case arg2 is a function
         ; See hash-ref docs for more info
         (#%app hash-ref efn arg1 (lambda () arg2))
         (#%app efn arg1 arg2))]))

;; Faster version for DrRacket
(define-syntax (arc-#%app stx)
  (syntax-parse
   stx
   [(_ fn:expr arg:expr)
    #'(#%app app-apply fn arg)]
   [(_ fn:expr arg1:expr arg2:expr)
    #'(#%app app-apply fn arg1 arg2)]
   [(_ args ...)
    #'(#%app args ...)]))

;(define-syntax (arc-#%app stx)
;  (syntax-parse
;   stx
;   [(_ fn:expr arg:expr)
;    #'(let ([efn fn])
;        (cond [(procedure? efn) (#%app efn arg)]
;              [(pair? efn) (#%app list-ref efn arg)]
;              [(string? efn) (#%app string-ref efn arg)]
;              [(hash? efn) (#%app hash-ref efn arg arc-nil)]
;              [else (#%app efn arg)]))]
;   [(_ fn:expr arg1:expr arg2:expr)
;    #'(let ([efn fn])
;        (if (hash? efn)
;            ; Have to use the lamda in case arg2 is a function
;            ; See hash-ref docs for more info
;            (#%app hash-ref efn arg1 (lambda () arg2))
;            (#%app efn arg1 arg2)))]
;   [(_ args ...)
;    #'(#%app args ...)]))

;;;
;;; Datums
;;;
;;; In Racket strings are immutable by default. Make them mutable for Arc.
;;;

;; TODO: Come back to this. Reader extensions required too?
;(define-syntax (arc-#%datum stx)
;  (syntax-parse
;   stx
;    [(_ . datum)
;     #:when (string? (syntax->datum #'datum))
;     #`(#%datum . #,(string-copy (syntax->datum #'datum)))]
;    [(_ . datum)
;     #'(#%datum . datum)]))

;; TODO: Override #%module-begin to
;; 1) unify namespaces
;; 2) allow set! without definition
;; 3) allow redefinition of constants

;;;
;;; Booleans
;;;
;;; Hide Arc's nil from Racket for performance wins. The pain is worth it.
;;;

; t is straightforward.
(idefine arc-t #t)

; nil means #f, '(), and void all at the same time. Most of the time #f is an
; acceptable standin, so default to that. In other cases use special checks to
; make it serve the inferred role.
;
; Note: It's not possible to create the pair (something . #f) in this
; implementation, since our cons will assume nil is terminating a list, not
; standing in for #f. Likewise, (something . void) is also impossible.
(idefine arc-nil #f)

(idefine (nil? value)
  (or (not value) (null? value) (void? value)))

(idefine (~nil? value)
  (not (nil? value)))

; Used when we want nil to be treated as '()
; Examples: cons, scdr
(idefine (nil->null value)
  (if (nil? value) null value))

; Used when we want nil to be treated as #f
; Examples: if, and, or, ...
(idefine (nil->#f value)
  (if (nil? value) #f value))

; Unfortunately, we can't implement 'and or 'or as arc-level macros since they
; need to hide our nil implementation.
; TODO: expose a true? or false? macro to arc that does the dirty work?

(define-syntax-rule (arc-and arg ...)
  (and (nil->#f arg) ...))

(define-syntax-rule (arc-or arg ...)
  (or (nil->#f arg) ...))

(idefine (is2 a b)
  (or (eqv? a b)
      (and (string? a) (string? b) (string=? a b))
      (and (nil? a) (nil? b))))

; In this case, 'is can't be an Arc macro because we want better parse errors
; and transparent pairwise grouping.
; TODO: Find a way to enable grouping macros in Arc.
(define-syntax (arc-is stx)
  (syntax-parse
   stx
   [(_ first rest ...+)
    #'(let ([tmp first])
        (and (is2 tmp rest) ...))]
   [(_ any ...)
    #'arc-t]))

; Supports Arc's ...&... ssyntax
(define-syntax (arc-and-ssyntax stx)
  (syntax-case stx ()
    ; Whether in expression or functional position, expand base usage
    ; to a lambda safe for application.
    [(_ (func ...))
     #'(lambda args
         (arc-and (apply func args) ...))]
    ; For performance, translate direct calls into direct calls.
    [(_ (func ...) args ...)
     (with-syntax ([(temps ...) (generate-temporaries #'(args ...))])
       #'(let-values ([(temps ...) (values args ...)])
           (arc-and (func temps ...) ...)))]))

;;;
;;; Control Flow
;;;

(define-syntax (arc-if stx)
  (syntax-parse 
   stx
   ; TODO: Special forms for two and three arguments?
   [(_ clause:expression-pair ...+)
    #'(cond [(nil->#f clause.a) clause.b] ...)]
   [(_ clause:expression-pair ...+ final:expr)
    #'(cond [(nil->#f clause.a) clause.b] ...
            [else final])]))

(define-syntax (arc-caselet stx)
  (syntax-parse
   stx
   [(_ var:id val:expr (~seq sym:expr result:expr) ...+)
    #'(let [(var val)]
        (case var [(sym) result] ...))]
   [(_ var:id val:expr (~seq sym:expr result:expr) ...+ else-expr:expr)
    #'(let [(var val)]
        (case val [(sym) result] ... [else else-expr]))]))

(define-syntax (arc-case stx)
  (syntax-parse
   stx
   [(_ val:expr (~seq sym:expr result:expr) ...+)
    #'(case val [(sym) result] ...)]
   [(_ val:expr (~seq sym:expr result:expr) ...+ else-expr:expr)
    #'(case val [(sym) result] ... [else else-expr])]))

(define-syntax (arc-catch stx)
  (syntax-parse
   stx
   [(_ body ...+)
    #'(call/cc (lambda (escape)
                 (syntax-parameterize ([throw (make-rename-transformer #'escape)])
                                      body ...)))]))

;;;
;;; Lists (Can't have a LISP without them)
;;;
;;; Careful application of nil->null and nil->#f allow us to keep Racket lists
;;; internally without Arc seeing them.
;;;
;;; Note: A pair in Arc can never contain #f or void. DO NOT VIOLATE THIS RULE
;;; or Racket functions will mysteriously fail when you accidentally give them
;;; #f instead of '()
;;;
;;; TODO: For now leave in redundant checks, but remove them later for better
;;; performance. Ex: (1 . void) is impossible, so use null->#f instead.
;;;
;;; TODO: Make sure cond condition ordering is optimal for most common use.
;;;
;;; TODO: Consider switching to mpairs instead.
;;;

(idefine (arc-cons head tail)
  (cons (nil->null head) (nil->null tail)))

(idefine (arc-car pair)
  (cond [(pair? pair) (nil->null (car pair))]
        ; (arc-car nil) -> nil
        ; (arc-car '()) -> nil
        [(nil? pair) null]
        [else (raise-type-error 'car "pair" pair)]))

(idefine (arc-cdr pair)
  (cond [(pair? pair) (nil->null (cdr pair))]
        ; (arc-cdr nil) -> nil
        ; (arc-cdr '()) -> nil
        [(nil? pair) null]
        [else (raise-type-error 'cdr "pair" pair)]))

(idefine (arc-append . args)    
  (apply append (map nil->null args)))

;; TODO: Should scar and scdr have the value in tail position?

(idefine (arc-scar pair car)
  (cond [(pair? pair) (unsafe-set-mcar! pair (nil->null car))]
        [(string? pair) (string-set! pair 0 car)]
        [else (raise-type-error 'scar "string or pair" 0 pair car)]))

(idefine (arc-scdr pair cdr)
  (if (pair? pair)
      (unsafe-set-mcdr! pair (nil->null cdr))
      (raise-type-error 'scdr "pair" 0 pair cdr)))

;;;
;;; Common syntax classes
;;;

(begin-for-syntax
  (define-syntax-class function-argument
    #:description "function argument"
    #:attributes (argument match-bind-expr)
    ; (fn ((o var default)) ...)
    (pattern ((~datum o)
              var:id
              (~optional default:expr #:defaults([default #'arc-nil])))
             #:with argument #'[var default]
             #:attr match-bind-expr #f)
    ; (fn (var) ...)
    (pattern argument:id
             #:attr match-bind-expr #f)
    ; (fn ((a b)) ...)
    ; (fn ((a b . c)) ...)
    (pattern match-bind-expr:match-target
             #:attr argument (datum->syntax #'var (gensym 'match-arg) #'var)))
  
  (define-syntax-class match-target
    #:description "destructuring target"
    (pattern var:id)
    (pattern ((~datum o) var:id (~optional default:expr)))
    (pattern (elt:match-target ...))
    (pattern (elt:match-target ... . rest:id)))
  
  (define-splicing-syntax-class match-binding-pair
    #:description "destructuring binding pair"
    (pattern (~seq var:match-target rhs:expr))))

;;;
;;; Destructuring assignment
;;;

(define-syntax (expand-extract-protect stx)
  (syntax-parse
   stx
   ;; Do nothing at end of list
   [(_ () src) #'()]
   ;; Assign remainder to lone variables
   [(_ var:id src:expr)
    #'((var src))]
   [(me (arg args ... . rest) src)
    #`(#,@(local-expand #'(expand-extract arg (arc-car src)) 'expression #f)
       #,@(local-expand #'(me (args ... . rest) (arc-cdr src)) 'expression #f))]))

(define-syntax (expand-extract stx)
  (syntax-parse
   stx
   ;; Handle defaults
   [(_ ((~datum o) var (~optional default #:defaults([default #'arc-nil]))) src)
    #'((var (arc-or src default)))]
   [(_ anything ... . goes)
    #`(#,@(local-expand #'(expand-extract-protect anything ... . goes) 'expression #f))]))

(define-syntax (expand-extract-let stx)
  (syntax-parse
   stx
   [(_ ()) #'()]
   [(me ([bind src] [binds srcs] ...))
    #`(#,@(local-expand #'(expand-extract bind src) 'expression #f)
       #,@(local-expand #'(me ([binds srcs] ...)) 'expression #f))]))

(define-syntax (extract-let stx)
  (syntax-parse
   stx
   [(me ([arg src] ...) body ...+)
    #'(me let* ([arg src] ...) body ...)]
   [(_ letform ([arg src] ...) body ...+)
    ;; Note: letform has to be let*/letrec because optional args can reference
    ;; prior bindings.
    #`(letform (#,@(local-expand #'(expand-extract-let ([arg src] ...))
                                 'expression
                                 #f))
        body ...)]))

;;;
;;; Lambda (fn)
;;;

(define-syntax (arc-fn stx)
  (syntax-parse
   stx
   [(_ (ignored ...)) #'void]
   [(_ (a:function-argument ... . rest) body ...+)
    (let ([matchers (filter-map (lambda (a b) (and (syntax? a) (cons a b)))
                                (attribute a.match-bind-expr)
                                (attribute a.argument))])
      (if (empty? matchers)
          #'(lambda (a.argument ... . rest) body ...)
          (with-syntax ([(match-target ...) (map car matchers)]
                        [(match-source ...) (map cdr matchers)])
            #'(lambda (a.argument ... . rest)
                (extract-let ([match-target match-source] ...)
                  body ...)))))]))

;;;
;;; Local Bindings (let, with, withs)
;;;
;;; TODO: Fix to support destructuring in top-level and module context
;;;

(define-syntax (def-with stx)
  (syntax-parse
   stx
   [(_ name:id let-form:id splicing-let-form:id)
    #'(define-syntax (name stx)
        (syntax-parse 
         stx
         ;; First try to match non-destructuring let
         [(_ (b:binding-pair (... ...)) body ...+)
          (with-syntax ([let-style (case (syntax-local-context)
                                     [(top-level module) #'splicing-let-form]
                                     [else #'let-form])])
            #'(let-style (b (... ...)) body (... ...)))]
         ;; Destructuring let
         [(_ (b:match-binding-pair (... ...)) body ...+)
          (with-syntax ([let-style (case (syntax-local-context)
                                     [(top-level module) #'splicing-letrec]
                                     [else #'let*])]
                        [(vals (... ...)) (generate-temporaries #'(b.rhs (... ...)))])
            ;; TODO: Don't create unneeded temporaries
            #'(extract-let let-style ([vals b.rhs] (... ...)
                                      [b.var vals] (... ...))
                           body (... ...)))]))]))

(def-with arc-with let splicing-let)

(def-with arc-withs let* splicing-letrec)

(define-syntax (arc-let stx)
  (syntax-parse 
   stx
   [(_ b:match-binding-pair body ...+)
    #'(arc-with b body ...)]))

;;;
;;; Definitions (def mac)
;;;

(define-syntax (arc-def stx)
  (syntax-parse
   stx
   [(_ name:id (ignored ...))
    (warn-on-redefinition #'name)
    #'(define name void)]
   [(_ name:id (a:function-argument ... . rest) body ...+)
    (warn-on-redefinition #'name)
    (let ([matchers (filter-map (lambda (a b) (and (syntax? a) (cons a b)))
                                (attribute a.match-bind-expr)
                                (attribute a.argument))])
      (if (empty? matchers)
          #'(define (name a.argument ... . rest) body ...)
          (with-syntax ([(match-target ...) (map car matchers)]
                        [(match-source ...) (map cdr matchers)])
            #'(define (name a.argument ... . rest)
                (extract-let ([match-target match-source] ...)
                  body ...)))))]))

(begin-for-syntax
  (define-splicing-syntax-class macro-argument
    #:description "macro argument"
    #:attributes (pattern)
    ;; TODO: Does this eat o's? let/with/fn/def did.
    (pattern ((~datum o)
              name:id
              (~optional default:expr #:defaults([default #'arc-nil])))
             #:attr pattern #'(~optional name #:defaults ([name #'default])))
    (pattern (~seq name:id)
             #:attr pattern #'name)
    (pattern ((~datum ^) elt:macro-argument ...+)
             #:attr pattern #'(~seq elt.pattern ...))
    (pattern (elt:macro-argument ...+)
             #:attr pattern #'(elt.pattern ...))))

(define-syntax (arc-mac stx)
  (define-splicing-syntax-class anemic-body
    #:description "minimal length body form"
    (pattern (~seq body:expr))
    (pattern (~seq e:expr ...+)
             #:attr body #'(begin e ...)))
  (syntax-parse
   stx
   [(_ name:id (arg ... . rest)
       (~optional (~seq #:leak (leaks:id ...)))
       body:anemic-body)
    #`(arc-cmac name
        #,@(if (attribute leaks) (list #'#:leak #'(leaks ...)) '())
        (arg ... . rest)
          body.body)]))

(define-syntax (arc-cmac stx)
  (syntax-parse
   stx
   [(_ name:id
       (~optional (~seq #:leak (leaks:id ...)))
       (~seq (arg:macro-argument ... . rest) body:expr) ...+
       (~do (warn-on-redefinition #'name)))
    #`(begin
        #,@(if (attribute leaks) (list #'(arc-leak leaks ...)) '())
        (define-syntax (name stx)
          (syntax-parse 
           stx
           [(_ arg.pattern ... . rest)
            #'body] ...)))]))

;;;
;;; Anaphoric Macros
;;;
;;; The current 'mac syntax does not allow for breaking hygiene, so these have
;;; to be defined here for the time being.
;;;
;;; TODO: Move these back to Arc now that mac can capture.
;;;

(define-syntax-rule (arc-afn (args ...) body ...)
  (letrec ([myself
            (syntax-parameterize ([self (make-rename-transformer #'myself)])
                                 (arc-fn (args ...) body ...))])
    myself))

(define-syntax-rule (it-lambda body ...)
  (lambda (val)
    (syntax-parameterize ([it (make-rename-transformer #'val)])
                         body ...)))

(define-syntax (arc-aif stx)
  (syntax-parse 
   stx
   ; TODO: Special forms for two and three arguments?
   [(_ clause:expression-pair ...+)
    #'(cond [(nil->#f clause.a) => (it-lambda clause.b)] ...)]
   [(_ clause:expression-pair ...+ final:expr)
    #'(cond [(nil->#f clause.a) => (it-lambda clause.b)] ...
            [else final])]))

; TODO: There has to be a better way...
; Technically it *is* tail recursive, just dirty.
(define-syntax (arc-aand stx)
  (syntax-parse 
   stx
   ; Note: The ordering of these forms is important
   [(_ a:expr b:expr ...+)
    #'(arc-aif a (arc-aand b ...) #f)]
   [(_ a:expr ...)
    #'(and a ...)]))

;; Sometimes you need an empty (do) form in Arc
(define-syntax (arc-do stx)
  (syntax-case stx ()
    [(_) #'(void)]
    [(_ body ...) #'(begin body ...)]))

;; If this works I wish I'd discovered it sooner!
(define-syntax (arc-leak stx)
  (syntax-parse
   stx
   [(_ name:id)
    (if (lexically-bound? #'name)
        #'(void)
        #'(define-syntax-parameter name
            (lambda (stx)
              (with-syntax ([source stx])
                #'(raise-syntax-error 
                   #f 
                   (istring name " can only be used as an ephemeral syntax parameter")
                   #'source)))))]
   [(self name:id ...)
    #'(begin (self name) ...)]))

(define-syntax (arc-bind stx)
  (syntax-parse
   stx
   [(_ pair:binding-pair body ...+)
    #'(arc-bind (pair.var pair.rhs) body ...)]
   [(_ (pair:binding-pair ...) body ...+)
    (with-syntax ([(temps ...) (generate-temporaries #'(pair.var ...))])
      #'(begin
          (let ([temps pair.rhs] ...)
          (syntax-parameterize ([pair.var (make-rename-transformer #'temps)] ...)
                               body ...))))]))
