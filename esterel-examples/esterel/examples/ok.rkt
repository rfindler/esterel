#lang racket
(require esterel/full)

(define (Aux I O)
  (every #:immediate (present? I)
         #:do
         (pause)
         (emit O)))

(define-signal S1 S2)

(define r
  (esterel
   (emit S1)
   (par (Aux S1 S2)
        (Aux S2 S1))))

#|

This program is from Gérard Berry; it's translation is above

module Aux :
input I;
output O;
every immediate I do
   pause;
   emit O
end every
end module 

module OK :
emit O1;
[
   run Aux [O1 / I , O2 / O]
||
   run Aux [O2 / I , O1 / O]
]
end module

end module

|#


;; this tests to make sure that we get
;; the right results in `N` iterations
(module+ main
  (define N 100)
  (for ([i (in-range N)])
    (define signals (react! r))
    (unless (equal? signals
                    (hash S1 (even? i)
                          S2 (odd? i)))
      (error 'ok.rkt
             "program is not ok\n  i: ~e\n  signals: ~e"
             i signals)))
  (printf "ok was ok for ~a iterations\n" N))
