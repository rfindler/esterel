#lang racket
(require "lang.rkt"
         "helpers.rkt"
         "must-can.rkt"
         redex/reduction-semantics)

(define-judgment-form L
  #:mode (--> I O O I O)
  #:contract (--> p E k E q)

  [--------------- "compl"
   (--> k · k E nothing)]

  [-------------------------------------------- "emit"
   (--> (! s) (extend · s tt) nothing E nothing)]

  [(--> p E_′ k E p_′) (lookup E s tt)
   ----------------------------------- "present+"
   (--> (? s p q) E_′ k E p_′)]

  [(--> q F_′ l E q_′) (lookup E s ff)
   ----------------------------------- "present-"
   (--> (? s p q) F_′ l E q_′)]

  [(--> p E_′ k E p_′) (where #t (≠ k nothing))
   ----------------------------------------------------------- "susp1"
   (--> (s ⊃ p) E_′ k E (seq (trap (loop (s ? pause (exit 0)))) (s ⊃ p)))]

  [(--> p E_′ nothing E p_′)
   ----------------------- "susp2"
   (--> (s ⊃ p) E_′ nothing E nothing)]

  [(--> p E_′ k E p_′) (where #t (≠ k nothing))
   -------------------------------------------- "seq1"
   (--> (seq p q) E_′ k E (seq p_′ q))]

  [(--> p E_′ nothing E p_′) (--> q F_′ l E q_′)
   --------------------------------------------- "seq2"
   (--> (seq p q) (∪ E_′ F_′) l E q_′)]

  [(--> p E_′ k E p_′) (where #t (≠ k nothing))
   -------------------------------------------- "loop"
   (--> (p *) E_′ k E (seq p_′ (p *)))]

  [(--> p E_′ k E p_′) (--> q F_′ l E q_′)
   ----------------------------------------------------- "parallel"
   (--> (par p q) (∪ E_′ F_′) (max k l) E (par p_′ q_′))]


  [(--> p E_′ k E p_′) (∈ k (set nothing (exit 0)))
   ------------------------------------------------ "trap1"
   (--> (trap p) E_′ nothing E nothing)]

  [(--> p E_′ k E p_′) (∉ k (set nothing (exit 0)))
   ------------------------------------------------ "trap2"
   (--> (trap p) E_′ (↓k k) E (trap p_′))]

  [(mc Must p (extend E s ⊥) (Pr S K)) (∈ s S) (--> p E_′ k (extend E s tt) p_′)
   ----------------------------------------------------------------------------- "csig+"
   (--> (p \\ s) (remove E_′ s) k E (p_′ \\ s))]

  [(mc Can+ p (extend E s ⊥) (Pr S K)) (∉ s S) (--> p E_′ k (extend E s ff) p_′)
   ----------------------------------------------------------------------------- "csig-"
   (--> (p \\ s) (remove E_′ s) k E (p_′ \\ s))])
