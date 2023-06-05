#lang racket
(require "lang.rkt"
         "helpers.rkt"
         "must-can.rkt"
         redex/reduction-semantics)
(provide --->)

(define-judgment-form L
  #:mode (---> I O O I O)
  ;; in the book, the S here is an E
  ;; but it is always mapping all signals
  ;; to tt and it uses ∪ so we just use
  ;; a set of signals instead
  #:contract (---> p S k E q)

  [--------------- "compl"
   (---> k · k E nothing)]

  [-------------------------------------------- "emit"
   (---> (! s) (s ·) nothing E nothing)]

  [(---> p S_′ k E p_′) (lookup E s tt)
   ----------------------------------- "present+"
   (---> (? s p q) S_′ k E p_′)]

  [(---> q S_′ l E q_′) (lookup E s ff)
   ----------------------------------- "present-"
   (---> (? s p q) S_′ l E q_′)]

  [(---> p S_′ k E p_′) (where #t (≠ k nothing))
   ----------------------------------------------------------- "susp1"
   (---> (s ⊃ p) S_′ k E (seq (trap ((? s pause (exit 0)) *)) (s ⊃ p_′)))]

  [(---> p S_′ nothing E p_′)
   ----------------------- "susp2"
   (---> (s ⊃ p) S_′ nothing E nothing)]

  [(---> p S_′ k E p_′) (where #t (≠ k nothing))
   -------------------------------------------- "seq1"
   (---> (seq p q) S_′ k E (seq p_′ q))]

  [(---> p S_p nothing E p_′) (---> q S_q l E q_′)
   --------------------------------------------- "seq2"
   (---> (seq p q) (∪ S_p S_q) l E q_′)]

  [(---> p S_′ k E p_′) (where #t (≠ k nothing))
   -------------------------------------------- "loop"
   (---> (p *) S_′ k E (seq p_′ (p *)))]

  [(---> p S_p k E p_′) (---> q S_q l E q_′)
   ----------------------------------------------------- "parallel"
   (---> (par p q) (∪ S_p S_q) (Max-kk k l) E (par p_′ q_′))]


  [(---> p S_′ k E p_′) (∈ k (set nothing (exit 0)))
   ------------------------------------------------ "trap1"
   (---> (trap p) S_′ nothing E nothing)]

  [(---> p S_′ k E p_′) (∉ k (set nothing (exit 0)))
   ------------------------------------------------ "trap2"
   (---> (trap p) S_′ (↓k k) E (trap p_′))]

  [(mc Must p (extend E s ⊥) (Pr S K)) (∈ s S) (---> p S_′ k (extend E s tt) p_′)
   ----------------------------------------------------------------------------- "csig+"
   (---> (p \\ s) (set- S_′ (set s)) k E (p_′ \\ s))]

  [(mc Can+ p (extend E s ⊥) (Pr S K)) (∉ s S) (---> p S_′ k (extend E s ff) p_′)
   ----------------------------------------------------------------------------- "csig-"
   (---> (p \\ s) (set- S_′ (set s)) k E (p_′ \\ s))])

(module+ test
  (test-judgment-holds (---> pause E pause · nothing))
  (test-judgment-holds (---> (! s) (s ·) nothing (extend · s tt) nothing))
  (test-judgment-holds (---> (? s pause nothing) · pause (s = tt ·) nothing))
  (test-judgment-holds (---> (? s pause nothing) · nothing (s = ff ·) nothing))
  (test-judgment-holds (---> (s ⊃ pause) · pause (s = tt ·) (seq (trap ((? s pause (exit 0)) *)) (s ⊃ nothing))))
  (test-judgment-holds (---> (s ⊃ nothing) · nothing (s = tt ·) nothing))
  (test-judgment-holds (---> (seq pause nothing) · pause · (seq nothing nothing)))
  (test-judgment-holds (---> (seq nothing pause) · pause · nothing))
  (test-judgment-holds (---> (pause *) · pause · (seq nothing (pause *))))
  (test-judgment-holds (---> (par nothing pause) · pause · (par nothing nothing)))
  (test-judgment-holds (---> (trap (seq (exit 0) pause)) · nothing · nothing))
  (test-judgment-holds (---> (trap pause) · pause · (trap nothing)))
  (test-judgment-holds (---> ((! s) \\ s) · nothing (s = tt ·) (nothing \\ s)))
  (test-judgment-holds (---> ((? s pause nothing) \\ s) · nothing (s = ff ·) (nothing \\ s)))

  (test-judgment-holds
   (---> ((((par (? SL1
                   (? SL2 (! SO1) (! SL3))
                   (? SL2 (! SO2) (! SL3)))
                (seq (! SL2)
                     (seq (? SL3 pause nothing)
                          (! SL1))))
           \\ SL3)
          \\ SL2)
         \\ SL1)
        (SO1 ·)
        nothing
        ·
        ((((par nothing nothing)
           \\ SL3)
          \\ SL2)
         \\ SL1))))
