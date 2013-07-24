#lang racket/base

(require (prefix-in arc- (file "reader.rkt")))

(provide configure)

(define (configure data)
  (current-readtable (arc-make-readtable))
  (current-read-interaction arc-read-syntax))
