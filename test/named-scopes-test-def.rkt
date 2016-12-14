#lang racket

(require (for-syntax debug-scopes/named-scopes
                     type-expander/debug-scopes ;;;
                     syntax/stx))

(begin-for-syntax
  (define-syntax-rule (named-transformer (_ stx) . body) (λ (stx) . body))
  (define (make-named-scope _) (make-syntax-introducer)))

(provide foo-macro bar-macro baz-macro)

(define-syntax (foo-macro stx)
  (syntax-case stx ()
    [(_ a)
     (let ([foo-scope (make-named-scope 'my-foo-scope-wohoo)])
       (foo-scope #'a))]))

(define-syntax bar-macro
  (named-transformer (bar-macro stx)
    #`(let ([x 1]) . #,(stx-cdr stx))))

(define-syntax (baz-macro stx)
  #`(let ([x 5]) . #,(stx-cdr stx)))
