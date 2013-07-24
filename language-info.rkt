#lang racket/base

(provide language-info)

(define (language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#((planet kogir/rark/runtime-config) configure #f))]
      [else default])))
