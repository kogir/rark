#lang racket/base

(require (for-syntax racket/base)
         racket/provide
         racket/require)

;;;
;;; Arc language macros, syntax, and functions
;;;

;; This hack keeps us from writing this out multiple times
(define-syntax-rule (export-arc phase ...)
  (begin 
    (begin 
      (require (for-meta phase
                         ;(prefix-in arc- (file "reader.rkt"))
                         (file "private/shared-syntax.rkt")
                         (file "private/core.rkt")
                         (file "private/functions.rkt")))
      
      (provide (for-meta phase
                         it
                         self
                         throw
                         ;TODO: Do I need these?
                         ;(rename-out [arc-read read]
                         ;            [arc-read-syntax read-syntax])
                         (filtered-out
                          (lambda (name)
                            (and (regexp-match? #rx"^arc-" name)
                                 (regexp-replace #rx"^arc-" name "")))
                          (combine-out (all-from-out 
                                        (file "private/shared-syntax.rkt")
                                        (file "private/core.rkt")
                                        (file "private/functions.rkt")))))))
    ...))

;(export-arc 0 1 2)
(export-arc 0 1)