#lang racket/base

;;;
;;; This module contains commonly used syntax classes.
;;; It is usually required for-syntax.
;;;

(require (for-syntax racket/base
                     racket/string)
         racket/performance-hint
         racket/stxparam
         syntax/parse)

(provide (all-defined-out))

;;;
;;; Shared syntax class definitions
;;;

(define-splicing-syntax-class binding-pair
  #:description "binding pair"
  (pattern (~seq var:id rhs:expr)))

(define-splicing-syntax-class expression-pair
  #:description "expression pair"
  (pattern (~seq a:expr b:expr)))

;;;
;;; Template syntax classes
;;;

(define-splicing-syntax-class template-declaration
  #:description "template name and includes"
  (pattern (~or (~seq name:id)
                (name:id include ...+))))

(define-splicing-syntax-class template-field
  #:description "template field definition"
  #:attributes (name value)
  (pattern (~seq name:id value:expr)))

;;;
;;; Anamorphic Macro Placeholders
;;;

(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error 
     #f 
     "it can only be used inside an anaphoric macro."
     stx)))

(define-syntax-parameter self
  (lambda (stx)
    (raise-syntax-error 
     #f 
     "self can only be used inside afn."
     stx)))

(define-syntax-parameter throw
  (lambda (stx)
    (raise-syntax-error 
     #f 
     "throw can only be used inside catch."
     stx)))

;;;
;;; Utility Functions
;;;

(define-syntax-rule (idefine body ...)
  (begin-encourage-inline
    (define body ...)))

; Inspired by Danny Yoo's arctangent
; https://github.com/dyoo/arctangent/blob/master/language.rkt
; Returns true if stx is an identifier that is lexically bound.
(define (lexically-bound? stx)
  (let ([expanded (local-expand stx (syntax-local-context) #f)])
    (cond [(not (identifier? expanded)) #t]
          [(identifier-binding expanded) #t]
          [else #f])))

(define (warn-on-redefinition id-stx)
  (when (lexically-bound? id-stx)
    (eprintf "*** redefining ~s\n" (syntax->datum id-stx))))

;; Sometimes you want to intern a literal composed at compile time
(define-for-syntax (coerce-string val)
  ;; TODO: print to a string instead?
  (cond [(string? val) val]
        [(number? val) (number->string val)]
        [(symbol? val) (symbol->string val)]
        [(char? val) (string val)]
        [(list? val) (string-join (map coerce-string val) " ")]))

(define-syntax (istring stx)
  (let* ([syntaxes (cdr (syntax->list stx))]
         [datums (map syntax->datum syntaxes)]
         [strings (map coerce-string datums)]
         [str (apply string-append strings)]
         [lit (datum-intern-literal str)])
    (datum->syntax stx lit stx stx)))
