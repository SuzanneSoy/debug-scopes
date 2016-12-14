#lang racket

(require ;"named-scopes-test-def.rkt"
         rackunit
         (for-syntax type-expander/debug-scopes
                     ;debug-scopes/named-scopes
                     ))

#|
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
|#

(define-syntax (quux stx)
  (syntax-case stx ()
    [(_ m)
     (let ()
       (displayln (+scopes #'m))
       (displayln (+scopes (syntax-local-introduce #'+)))
       (print-full-scopes)
       (syntax-local-introduce #'+))]))
(quux -)