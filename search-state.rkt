#lang racket
(require "structs.rkt")
(module+ test (require rackunit))

#|

This provides an API to select a set from a
powerset of some given elements. The elements
are specified with `add-element!`. The choose
operation returns a subset of the elements with
the constraint that it is not a super set of
any of the sets that have been passed to
`add-subset`. If there is no such subset,
it returns #f.

Currently the implementation considers only
singletons and returns #f when it cannot find
one of those.

idea:
 - take the smallest subset that's known to not fail and add one element to it...after a failure
 - after a success, just add one element to the previous success

|#


(provide
 (contract-out
  [new-search-state (-> search-state?)]
  [continue! (->i ([s search-state?]
                   [rollback any/c]
                   [determined (listof signal?)]
                   [choices (non-empty-listof signal?)])
                  [choice signal?])]
  [fail! (->i ([s search-state?])
              (values [rollback any/c] [choice (or/c #f signal?)]))]
  ))

(struct choice-point (rollback determined choices choice))
;; successes : (listof (cons/c rollback (listof signal?)))
(struct search-state (successes) #:mutable)
(define (new-search-state) (search-state (set)))

(define (continue! se-st rollback determined choices)
  (set-search-state-successes!
   se-st
   (cons (choice-point rollback determined choices (car choices))
         (search-state-successes se-st)))
  (car choices))

(define (fail! se-st)
  (void))

(module+ test
  (define s1 (signal))
  (define s2 (signal))
  
  (check-equal?
   (let ([se-st (new-search-state)])
     (continue! se-st 1 '() (list s1)))
   s1)

  (check-equal?
   (call-with-values
    (λ ()
      (let ([state (new-search-state)])
        (fail! (list s1))))
    list)
   (list s1)))

