#lang racket
(require "lang.rkt"
         "helpers.rkt"
         "must-can-hat.rkt"
         redex/reduction-semantics)

(provide -->^)

(define-judgment-form L
  #:mode (-->^ I O O I O)
  ;; in the book, the S here is an E
  ;; but it is always mapping all signals
  ;; to tt and it uses ∪ so we just use
  ;; a set of signals instead
  #:contract (-->^ e S k E e)

  [--------------- "compl"
   (-->^ k · k E nothing)]

  [-------------------------------------------- "emit"
   (-->^ (! s) (s ·) nothing E nothing)]

  [(lookup E s B)
   ---------------------- "present"
   (-->^ s · nothing E B)]

  [(-->^ e_1 S_1′ nothing E tt) (-->^ e_2 S_2′ k E e_2′)
   ----------------------------------------------------- "if-tt"
   (-->^ (if e_1 e_2 e_3) (∪ S_1′ S_2′) k E e_2′)]

  [(-->^ e_1 S_1′ nothing E ff) (-->^ e_3 S_3′ k E e_3′)
   ----------------------------------------------------- "if-ff"
   (-->^ (if e_1 e_2 e_3) (∪ S_1′ S_3′) k E e_3′)]

  [(-->^ e_1 S_1′ k E e_1′) (where #t (≠ k nothing))
   ------------------------------------------------- "if-non-nothing"
   (-->^ (if e_1 e_2 e_3) S_1′ k E e_1′)]

  [(-->^ e S_′ k E e_′) (where #t (≠ k nothing))
   ----------------------------------------------------------- "susp1"
   (-->^ (s ⊃ e) S_′ k E (seq (trap ((if s pause (exit 0)) *)) (s ⊃ e_′)))]

  [(-->^ e S_′ nothing E e_′)
   -------------------------- "susp2"
   (-->^ (s ⊃ e) S_′ nothing E nothing)]

  [(-->^ e_1 S_′ k E e_′) (where #t (≠ k nothing))
   ----------------------------------------------- "seq1"
   (-->^ (seq e_1 e_2) S_′ k E (seq e_′ e_2))]

  [(-->^ e_1 S_1 nothing E e_′) (-->^ e_2 S_2 l E e_2′)
   --------------------------------------------- "seq2"
   (-->^ (seq e_1 e_2) (∪ S_1 S_2) l E e_2′)]

  [(-->^ e S_′ k E e_′) (where #t (≠ k nothing))
   -------------------------------------------- "loop"
   (-->^ (e *) S_′ k E (seq e_′ (e *)))]

  [(-->^ e_1 S_1 k E e_1′) (-->^ e_2 S_2 l E e_2′)
   ----------------------------------------------------- "parallel"
   (-->^ (par e_1 e_2) (∪ S_1 S_2) (Max-kk k l) E (par e_1′ e_2′))]


  [(-->^ e S_′ k E e_′) (∈ k (set nothing (exit 0)))
   ------------------------------------------------ "trap1"
   (-->^ (trap e) S_′ nothing E nothing)]

  [(-->^ e S_′ k E e_′) (∉ k (set nothing (exit 0)))
   ------------------------------------------------ "trap2"
   (-->^ (trap e) S_′ (↓k k) E (trap e_′))]

  [(mc^ Must e (extend E s ⊥) (Pr S K)) (∈ s S) (-->^ e S_′ k (extend E s tt) e_′)
   ----------------------------------------------------------------------------- "csig+"
   (-->^ (e \\ s) (set- S_′ s) k E (e_′ \\ s))]

  [(mc^ Can+ e (extend E s ⊥) (Pr S K)) (∉ s S) (-->^ e S_′ k (extend E s ff) e_′)
   ----------------------------------------------------------------------------- "csig-"
   (-->^ (e \\ s) (set- S_′ s) k E (e_′ \\ s))])


(module+ test
  #;
  (test-judgment-holds (-->^ ((+ 3 (if s 1 2)) \\ s)
                             · nothing ·
                             (nothing \\ s)))
  #;
  (test-judgment-holds (-->^ ((+ (seq pause 3) (if s 1 2)) \\ s)
                             · nothing ·
                             ((+ (seq nothing 3) (if s 1 2)) \\ s)))
  #;
  (test-judgment-holds (-->^ ((+ (if s 1 2) (seq pause 3)) \\ s)
                             · nothing ·
                             ((+ (if s 1 2) (seq nothing 3)) \\ s))))
