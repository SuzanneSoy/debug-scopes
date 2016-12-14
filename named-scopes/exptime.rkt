#lang racket

(require (for-template '#%kernel)
         debug-scopes
         racket/syntax
         racket/struct
         debug-scopes)

(provide make-named-scope
         named-transformer
         (rename-out [-syntax-local-introduce syntax-local-introduce]))

(define (make-named-scope nm)
  (define name (if (symbol? nm) nm (string->symbol nm)))
  (define E1
    (local-expand (datum->syntax #f
                                 `(,#'module
                                   ,name
                                   debug-scopes/named-scopes/dummy-lang
                                   '#%kernel
                                   list))
                  'top-level
                  (list)))
  (define/with-syntax (_module _name _lang (_modbeg (_#%require QK1) Body1)) E1)
  (define QK (datum->syntax #'QK1 'qk-sym))
  (define Body (datum->syntax #'Body1 'body-sym))
  (define Zero (datum->syntax #f 'zero))
  (define ΔBody (make-syntax-delta-introducer Body Zero))
  (define QK-Body (ΔBody QK 'remove))
  (define ΔQK-Body (make-syntax-delta-introducer QK-Body Zero))
  (define QK-rest (ΔQK-Body QK 'remove))
  (define named-scope (make-syntax-delta-introducer QK-rest Zero))
  named-scope)

(define ((has-scope scope) stx)
  (and (identifier? stx)
       (bound-identifier=? stx (scope stx 'add))))

(define (replace-scope old new)
  (define (replace e)
    (cond
      [(syntax? e)
       (datum->syntax (if ((has-scope old) e)
                          (new (old e 'remove) 'add)
                          e)
                      (replace (syntax-e e))
                      e
                      e)]
      [(pair? e) (cons (replace (car e)) (replace (cdr e)))]
      [(vector? e) (list->vector (replace (vector->list e)))]
      [(hash? e)
       (cond [(hash-eq? e) (make-hasheq (replace (hash->list e)))]
             [(hash-eqv? e) (make-hasheqv (replace (hash->list e)))]
             [(hash-equal? e) (make-hash (replace (hash->list e)))]
             [else e])]
      [(prefab-struct-key e)
       => (λ (k)
            (apply make-prefab-struct k (replace (struct->list e))))]
      [else e]))
  replace)

(define (deep-has-scope sc)
  (define (scan e)
    (cond
      [(syntax? e) (or ((has-scope sc) e) (scan (syntax-e e)))]
      [(pair? e) (or (scan (car e)) (scan (cdr e)))]
      [(vector? e) (scan (vector->list e))]
      [(hash? e) (scan (hash->list e))]
      [(prefab-struct-key e) (scan (struct->list e))]
      [else #f]))
  scan)

(define (old-macro-scope)
  (make-syntax-delta-introducer
   (syntax-local-identifier-as-binding
    (syntax-local-introduce
     (datum->syntax #f 'zero)))
   (datum->syntax #f 'zero)))

(define (convert-macro-scopes stx)
  (if (sli-scopes)
      (let* ([macro (sli-scopes)]
             [old-macro (old-macro-scope)])
        ((replace-scope old-macro macro) stx))
      ;; Otherwise leave unchanged.
      stx))

(define ((named-transformer-wrap name f) stx)
  (parameterize ([sli-scopes (make-named-scope (format "macro: ~a" name))])
    ;;; TODO: we should detect the presence of old-* here instead, and 'add them
    (let ([res (f (convert-macro-scopes stx))])
      (when ((deep-has-scope (old-macro-scope)) res)
        (error (format "original macro scope appeared within the result of a named transformer: ~a\n~a\n~a"
                       res
                       (+scopes res)
                       (with-output-to-string (λ () (print-full-scopes))))))
      ((old-macro-scope) ((sli-scopes) res 'flip) 'add))))

(define-syntax-rule (named-transformer (name stx) . body)
  (named-transformer-wrap 'name (λ (stx) . body)))

(define sli-scopes (make-parameter #f))

(define (-syntax-local-introduce stx)
  (if (sli-scopes)
      ((sli-scopes) stx 'flip)
      (syntax-local-introduce stx)))
