#lang racket
(provide (rename-out [my-module-begin #%module-begin]))
(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ real-lang body)
     (syntax-case (local-expand #'(module m real-lang body) 'top-level (list)) ()
       [(module nm lng (#%plain-module-begin . body2))
        #`(#%plain-module-begin
             (#%require real-lang)
             . #,(values #'body2))])]))