#lang racket

(require (for-syntax "exptime.rkt"))

(provide (rename-out [-define-syntax define-syntax]))

(define-syntax (-define-syntax stx)
  (syntax-case stx ()
    [(_ (name arg) . body) #'(define-syntax name
                               (named-transformer (name arg)
                                 . body))]
    [(_ name value) #'(define-syntax name value)]))