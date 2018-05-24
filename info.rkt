#lang info
(define collection "debug-scopes")
(define deps '("base"
               "pretty-format"
               "rackunit-lib"
               "reprovide-lang"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "scribble-enhanced"))
(define scribblings '(("scribblings/debug-scopes.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
