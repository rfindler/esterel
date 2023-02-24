#lang racket
(require esterel/full)

#|

This is a translation of the code in 1.6 of
_Compiling Esterel_; the test cases are as
given in figure 1.2

|#

(define-signals A B R O)

(define abro
  (reaction
   (loop (par (await (present? A))
              (await (present? B)))
         (emit O)
         #:each (present? R))))

(module+ test
  (define (react/o? #:emit [signals '()])
    (hash-ref (react! abro #:emit signals) O #f))
  (require rackunit)

  (check-equal? (react/o?)                     #f)
  (check-equal? (react/o? #:emit (list A B))   #t)
  (check-equal? (react/o? #:emit (list A))     #f)
  (check-equal? (react/o? #:emit (list R))     #f)
  (check-equal? (react/o? #:emit (list A))     #f)
  (check-equal? (react/o? #:emit (list B))     #t)
  (check-equal? (react/o? #:emit (list A B R)) #f))
