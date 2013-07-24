#lang s-exp syntax/module-reader
(planet kogir/rark/load)
#:read arc-read
#:read-syntax arc-read-syntax
#:wrapper1 arc-read-wrapper
#:language-info '#((planet kogir/rark/language-info) language-info #f)

(require (prefix-in arc- (planet kogir/rark/reader)))

;; Idea and method ripped from racket/load/lang/reader.rkt
