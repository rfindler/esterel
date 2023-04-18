#lang racket
(require "must-can-star.rkt" redex pict)

(define (mc-rewrite lws)
  (list ""
        (list-ref lws 2)
        "[" (list-ref lws 3) "," (list-ref lws 4) "] = "
        (list-ref lws 5)
        ""))

(define (binop-rewrite lws)
  (list ""
        (list-ref lws 2)
        (~a (lw-e (list-ref lws 1)))
        (list-ref lws 3)
        ""))

(define (set--rewrite lws)
  (list ""
        (list-ref lws 2)
        "\\"
        (list-ref lws 3)
        ""))


(define (set-rewrite lws)
  (append '("{")
          (drop-last (cdr (cdr lws)))
          '("}")))
(define (drop-last l) (reverse (cdr (reverse l))))

(define (Pr-rewrite lws)
  (list "⟨"
        (list-ref lws 2)
        ","
        (list-ref lws 3)
        "⟩"))

(define (op-rewrite lws)
  (list "{ x "
        (list-ref lws 2)
        " y | x ∈ "
        (list-ref lws 3)
        " and y ∈ "
        (list-ref lws 4)
        "}"))

(define (extend-rewrite lws)
  (list ""
        (list-ref lws 2)
        " + "
        (list-ref lws 3)
        " = "
        (list-ref lws 4)
        ""))

(define (rule->pict rule)
  (with-compound-rewriters (['mc* mc-rewrite]
                            ['∈ binop-rewrite]
                            ['∉ binop-rewrite]
                            ['∪ binop-rewrite]
                            ['set set-rewrite]
                            ['set- set--rewrite]
                            ['Pr Pr-rewrite]
                            ['op-each-pair op-rewrite]
                            ['extend extend-rewrite])
    (parameterize ([judgment-form-cases (list rule)])
      (render-judgment-form mc*))))

(module+ main
  (define counts '(4 3 1 1 1 3 1 1 1 1 1 1 1 2 2))
  (apply
   vc-append
   20
   (let loop ([counts counts]
              [names (judgment-form->rule-names mc*)])
     (cond
       [(null? counts)
        (cond
          [(null? names) '()]
          [else
           (loop (cons 1 counts) names)])]
       [else
        (define line (car counts))
        (cons (apply hc-append 20
                     (for/list ([name (in-list names)]
                                [i (in-range (car counts))])
                       (rule->pict name)))
              (loop (cdr counts)
                    (drop names (car counts))))]))))
