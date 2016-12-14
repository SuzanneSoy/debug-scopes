#lang racket

(require (for-syntax "exptime.rkt"
                     debug-scopes/named-scopes-sli-parameter))

(define-for-syntax (use-site-context?)
  (not (bound-identifier=? (syntax-local-introduce #'here)
                           (syntax-local-identifier-as-binding
                            (syntax-local-introduce #'here)))))

(provide (rename-out [-define-syntax define-syntax])
         (for-syntax
          (rename-out [-syntax-local-introduce syntax-local-introduce])))

(define-syntax (-define-syntax stx)
  (syntax-case stx ()
    [(_ (name arg) . body) #'(define-syntax name
                               (named-transformer (name arg)
                                 . body))]
    [(_ name value) #'(define-syntax name value)]))

(define-for-syntax (-syntax-local-introduce stx)
  (define /m (if (sli-scopes)
                 ((sli-scopes) stx 'flip)
                 (syntax-local-introduce stx)))
  (if (use-site-context?)
      (let* ([zero (datum->syntax #f 'zero)]
             [sli (syntax-local-introduce zero)]
             [sli-use (syntax-local-identifier-as-binding sli)]
             [+sli (make-syntax-delta-introducer sli zero)]
             [+sli-use (make-syntax-delta-introducer sli-use zero)]
             [use (+sli-use sli 'remove)]
             [+use (make-syntax-delta-introducer use zero)])
        (+use /m))
      /m))
