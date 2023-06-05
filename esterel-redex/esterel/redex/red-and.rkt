#lang racket
(require "lang.rkt"
         "helpers.rkt"
         redex/reduction-semantics)

(provide -->& -->&*)

(define-judgment-form L
  #:mode (-->& I I O O)
  #:contract (-->& e E e E)

  [------------------------------------------- "emit"
   (-->& (in-hole EC (! s)) E
         (in-hole EC nothing) (extend E s tt))]

  [(lookup E s B)
   ----------------------- "signal"
   (-->& (in-hole EC s) E
         (in-hole EC B) E)]

  [---------------------------- "suspend done"
   (-->& (in-hole EC (s ⊃ v)) E
         (in-hole EC v) E)]

  [------------------------------ "seq"
   (-->& (in-hole EC (seq v e)) E
         (in-hole EC e) E)]

  [----------------------------------- "*"
   (-->& (in-hole EC (e *)) E
         (in-hole EC (seq e (e *))) E)]

  [(no-trap-context EC_2)
   ----------------------------------------------------- "exit"
   (-->& (in-hole EC_1 (trap (in-hole EC_2 (exit N)))) E
         (in-hole EC_1 (↓k (exit N))) E)]

  [------------------------------- "trap done"
   (-->& (in-hole EC_1 (trap v)) E
         (in-hole EC_1 v) E)]


  [------------------------------------ "if-tt"
   (-->& (in-hole EC (if tt e_1 e_2)) E
         (in-hole EC e_1) E)]

  [------------------------------------ "if-f"
   (-->& (in-hole EC (if ff e_1 e_2)) E
         (in-hole EC e_2) E)]

  [------------------------------------ "op"
   (-->& (in-hole EC (op v_1 v_2)) E
         (in-hole EC (δ op v_1 v_2)) E)])

(define-judgment-form L
  #:contract (blocked e E S)
  #:mode (blocked I I O)

  [-------------
   (blocked v E ·)]

  [(lookup E s ⊥)
   ------------------- "sig"
   (blocked s E (s ·))]

  [(blocked e_1 E S)
   ---------------------------- "if"
   (blocked (if e_1 e_2 e_3) E S)]

  [(blocked e_1 E S_1) (blocked e_1 E S_2)
   --------------------------------------- "op"
   (blocked (op e_1 e_2) E (∪ S_1 S_2))]

  [(blocked e E S) (where #t (≠ S ·))
   ---------------------------------- "⊃"
   (blocked (s ⊃ e) E S)]

  [(blocked e E S) (where #t (≠ S ·))
   ---------------------------------- "trap"
   (blocked (trap e) E S)])
  
(define-judgment-form L
  #:mode (-->&* I I O O)
  #:contract (-->&* e E e E)

  [(done e)
   --------------- "refl"
   (-->&* e E e E)]

  [(-->& e_1 E_1 e_2 E_2) (-->&* e_2 E_2 e_3 E_3)
   ---------------------------------------------- "step"
   (-->&* e_1 E_1 e_3 E_3)])

(define-judgment-form L
  #:contract (done e)
  #:mode (done I)
  [------- "N"
   (done N)]

  [------- "B"
   (done B)]

  [-------------- "nothing"
   (done nothing)]

  [------------ "pause"
   (done pause)]

  [(done e_1) (done e_2)
   -------------------- "par"
   (done (par e_1 e_2))]

  [(done e_1)
   -------------------- "seq"
   (done (seq e_1 e_2))])
  
(define-judgment-form L
  #:mode (no-trap-context I)
  #:contract (no-trap-context EC)
  
  [----
   (no-trap-context hole)]
  [(no-trap-context EC)
   ---
   (no-trap-context (s ⊃ EC))]
  [(no-trap-context EC)
   ---
   (no-trap-context (seq EC e))]
  [(no-trap-context EC)
   ---
   (no-trap-context (par EC e))]
  [(no-trap-context EC)
   ---
   (no-trap-context (par e EC))]
  [(no-trap-context EC)
   ---
   (no-trap-context (if EC e_1 e_2))]
  [(no-trap-context EC)
   ---
   (no-trap-context (op EC e))]
  [(no-trap-context EC)
   ---
   (no-trap-context (op v EC))])

(module+ test
  (test-judgment-holds
   (-->&* (if (< 1 2) (! O1) (! O2))
          ·
          nothing
          (O1 = tt ·)))

  (test-judgment-holds
   (-->&* (seq (trap (+ 2 (trap (if (exit 1) (! O1) (! O2))))) 3)
          ·
          3
          ·))

  (test-judgment-holds
   (-->&* (+ (trap (+ 2 3)) 4)
          ·
          9
          ·))

  (test-judgment-holds
   (-->&* (pause *)
          ·
          (seq pause (pause *))
          ·)))
