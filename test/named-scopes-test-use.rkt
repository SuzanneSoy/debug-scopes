#lang racket

(require "named-scopes-test-def.rkt"
         rackunit)

(define r1 (foo-macro +))
(define r2 (let ([x 2])
                (bar-macro x)))
(define r3 (let ([x 3])
                (baz-macro x)))

(define r4 (let ()
             (define-syntax (quux stx)
               (syntax-local-introduce #'+))
             (quux)))

(check-equal? (list r1 r2 r3 r4) (list + 2 3 +))
