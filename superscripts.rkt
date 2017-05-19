#lang racket

(require racket/syntax
         racket/string
         racket/format
         debug-scopes/named-scopes-sli-parameter)

(provide +scopes print-full-scopes)

(define max-seen-scopes 0)
(define seen-scopes (make-hash))

(define (print-full-scopes [reset? #t])
  (define scopes (sort (hash->list seen-scopes) < #:key cadr))
  (define l
    (map (λ (s)
           (format "~a ~a"
                   (cadr s)
                   (string-join (map ~a (cdr (vector->list (cddr s))))
                                " ")))
         scopes))
  (define max-len (apply max 0 (map string-length l)))
  (define (pad str)
    (string-append
     str
     (make-string (- max-len (string-length str)) (string-ref " " 0))))
  (for-each (λ (s str)
              (printf "~a ~a\n"
                      (pad str)
                      (vector-ref (cddr s) 0)))
            scopes
            l)
  (when reset?
    (hash-clear! seen-scopes)
    (set! max-seen-scopes 0)))

(define (string-replace* str replacements)
  (if (null? replacements)
      str
      (string-replace* (string-replace str
                                       (caar replacements)
                                       (cadar replacements))
                       (cdr replacements))))

(define (digits->superscripts str)
  (string-replace* str '(["0" "⁰"]
                         ["1" "¹"]
                         ["2" "²"]
                         ["3" "³"]
                         ["4" "⁴"]
                         ["5" "⁵"]
                         ["6" "⁶"]
                         ["7" "⁷"]
                         ["8" "⁸"]
                         ["9" "⁹"])))

(define (digits->subscripts str)
  (string-replace* str '(["0" "₀"]
                         ["1" "₁"]
                         ["2" "₂"]
                         ["3" "₃"]
                         ["4" "₄"]
                         ["5" "₅"]
                         ["6" "₆"]
                         ["7" "₇"]
                         ["8" "₈"]
                         ["9" "₉"])))

(define (change-digits1 l [mode #t])
  (if (null? l)
      '()
      (cons ((if mode digits->superscripts digits->subscripts) (car l))
            (change-digits1 (cdr l) (not mode)))))

(define (change-digits2 l)
  (let ([min-id (apply min l)]
        [max-id (apply max l)])
    (format "~a˙˙~a~a"
            (digits->superscripts (~a min-id))
            (digits->superscripts (~a max-id))
            (string-join (map (λ (x)
                                (format "⁻~a" (digits->superscripts (~a x))))
                              (filter-not (λ (x) (member x l))
                                          (range min-id (add1 max-id))))
                         ""))))

(define (change-digits l)
  (let ([a (string-join (change-digits1 (map ~a l)) "")])
    (if (null? l)
        a
        (let ([b (change-digits2 l)])
          (if (or (and (< (string-length a) (string-length b))
                       (> (string-length a) 4))
                  (= (length l) 1))
              a
              b)))))

(define (extract-scope-ids e)
  (map (λ (c)
         (car (hash-ref! seen-scopes (vector-ref c 0)
                         (λ ()
                           (begin0 (cons max-seen-scopes c)
                                   (set! max-seen-scopes
                                         (add1 max-seen-scopes)))))))
       (hash-ref (syntax-debug-info e) 'context)))

(define (add-scopes e)
  (cond
    [(identifier? e)
     (let ([ids (extract-scope-ids e)])
       ;(format-id e "~a⁽~a⁾" e (string-join (map digits->superscripts
       ;                                          (map ~a ids)) " ")))
       (format-id e "~a~a" e (change-digits ids)))]
    [(syntax? e) (datum->syntax e (add-scopes (syntax-e e)) e e)]
    [(pair? e) (cons (add-scopes (car e))
                     (add-scopes (cdr e)))]
    [else e]))

(define (sli/use whole-stx)
  ;(…)ˢˡⁱ⁼ ᵘˢᵉ⁼
  ;(…)ₛₗᵢ₌ ᵤₛₑ₌
  (if (syntax-transforming?)
      (let* ([stx (datum->syntax whole-stx 'to-id)]
             [sli (syntax-local-introduce stx)]
             [stx-ids (extract-scope-ids stx)]
             [sli-ids (extract-scope-ids sli)]
             [stx-slb (syntax-local-identifier-as-binding stx)]
             [sli-slb (syntax-local-identifier-as-binding sli)]
             [stx-binding (extract-scope-ids stx-slb)]
             [sli-binding (extract-scope-ids sli-slb)]
             [use (append (set-symmetric-difference stx-ids stx-binding)
                          (set-symmetric-difference sli-ids sli-binding))]
             [stx/sli-use (set-subtract (set-symmetric-difference stx-ids
                                                                  sli-ids)
                                        use)])
        (format "ˢˡⁱ⁼~a⁺ᵘˢᵉ⁼~a~a"
                (string-join (map digits->superscripts (map ~a stx/sli-use))
                             " ")
                (string-join (map digits->superscripts (map ~a use))
                             " ")
                (if (sli-scopes)
                    (let* ([named ((sli-scopes) (datum->syntax #f 'zero))]
                           [named-scope-id (extract-scope-ids named)])
                      (format "⁽ⁿᵃᵐᵉᵈ⁼~a⁾"
                              (string-join (map digits->superscripts
                                                (map ~a named-scope-id))
                                           " ")))
                    "")))
      ""))

(define (+scopes stx)
  (format "~a~a"
          (syntax->datum (add-scopes stx))
          (sli/use stx)))

#;(define-syntax (foo stx)
    (displayln (+scopes stx))
    #'(void))

#;(foo a)