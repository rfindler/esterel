#lang racket
(require "must-can-star.rkt" "lang.rkt"
         redex pict)
(provide format-rules grammar)

(define (mc-rewrite lws)
  (list ""
        (list-ref lws 2)
        "[" (list-ref lws 3) ", " (list-ref lws 4) "] = "
        (list-ref lws 5)
        ""))

(define (binop-rewrite lws)
  (list ""
        (list-ref lws 2)
        (~a " " (lw-e (list-ref lws 1)) " ")
        (list-ref lws 3)
        ""))

(define (set--rewrite lws)
  (list ""
        (list-ref lws 2)
        "\\"
        (list-ref lws 3)
        ""))

(define (parens-rewrite lws)
  (list "("
        (list-ref lws 2)
        ")"))

(define (set-rewrite lws)
  (append '("{")
          (drop-last (cdr (cdr lws)))
          '("}")))
(define (drop-last l) (reverse (cdr (reverse l))))

(define (Pr-rewrite lws)
  (list "⟨"
        (list-ref lws 2)
        ", "
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

(define (lookup-rewrite lws)
  (list ""
        (list-ref lws 2)
        "("
        (list-ref lws 3)
        ") = "
        (list-ref lws 4)
        ""))

(define (rule->pict rule)
  (with-the-rewriters
      (λ ()
        (parameterize ([judgment-form-cases (list rule)])
          (render-judgment-form mc*)))))

(define (grammar)
  (define nts
    (remove* '(p q Can fn fn+ set K K* S E s N R)
             (language-nts L)))
  (with-the-rewriters
      (λ () (render-language L #:nts nts))))
  
(define (with-the-rewriters thunk)
  (with-compound-rewriters (['mc* mc-rewrite]
                            ['∈ binop-rewrite]
                            ['∉ binop-rewrite]
                            ['∪ binop-rewrite]
                            ['set set-rewrite]
                            ['set- set--rewrite]
                            ['Pr Pr-rewrite]
                            ['op-each-pair op-rewrite]
                            ['extend extend-rewrite]
                            ['extend* extend-rewrite]
                            ['lookup lookup-rewrite]
                            ['lookup* lookup-rewrite]
                            ['lookup*-B⊥ lookup-rewrite]
                            ['parens parens-rewrite])
    (thunk)))

(define (format-rules width height mandatory-breaks
                      #:rules [rules (judgment-form->rule-names mc*)]
                      #:horizontal-gap [horizontal-gap 40]
                      #:vertical-gap [vertical-gap 20])
  (define line (blank))
  (define pict (blank))
  (let loop ([rules rules])
    (cond
      [(null? rules)
       (values #f (vc-append vertical-gap pict line))]
      [else
       (define fst-pict (rule->pict (car rules)))
       (cond
         [(and (<= (+ (pict-width line) horizontal-gap (pict-width fst-pict))
                   width)
               (not (member (car rules) mandatory-breaks)))
          (set! line (hbl-append horizontal-gap line fst-pict))
          (loop (cdr rules))]
         [else
          (set! pict (vc-append vertical-gap pict line))
          (set! line fst-pict)
          (cond
            [(<= (pict-height pict) height)
             (loop (cdr rules))]
            [else
             (values rules pict)])])])))

(module+ main
  (define counts '(4 3 1 1 1 3 1 1 1 1 1 1 1 2 2))
  (define p
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
  (pict-width p))
