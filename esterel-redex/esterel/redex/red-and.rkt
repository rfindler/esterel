#lang racket
(require "lang.rkt"
         "helpers.rkt"
         redex/reduction-semantics)

(provide -->& -->&*)

(define-judgment-form L
  #:mode (-->&* I I I O O O)
  #:contract (-->&* e E S⊥ e E S⊥)

  [(done e)
   --------------- "refl"
   (-->&* e E S⊥ e E S⊥)]

  [(-->& e_1 E_1 S⊥_1 e_2 E_2 S⊥_2) (-->&* e_2 E_2 S⊥_2 e_3 E_3 S⊥_3)
   ------------------------------------------------------------------ "step"
   (-->&* e_1 E_1 S⊥_1 e_3 E_3 S⊥_3)])

(define-judgment-form L
  #:mode (-->& I I I O O O)
  #:contract (-->& e E S⊥ e E S⊥)

  [--------------------------------------------- "emit+"
   (-->& (in-hole EC (! s)) E ⊥
         (in-hole EC nothing) (extend E s tt) ⊥)]

  [------------------------------------------- "emitS"
   (-->& (in-hole EC (! s)) E S
         (in-hole EC nothing) E (∪ S (set s)))]

  [(lookup E s B)
   --------------------------- "signal"
   (-->& (in-hole EC s) E S⊥
         (in-hole EC B) E S⊥)]

  [------------------------------- "suspend done"
   (-->& (in-hole EC (s ⊃ v)) E S⊥
         (in-hole EC v) E S⊥)]

  [--------------------------------- "seq"
   (-->& (in-hole EC (seq v e)) E S⊥
         (in-hole EC e) E S⊥)]

  [------------------------------------- "*"
   (-->& (in-hole EC (e *)) E S⊥
         (in-hole EC (seq e (e *))) E S⊥)]

  [(no-trap/par-context EC_2)
   -------------------------------------------------------- "exit"
   (-->& (in-hole EC_1 (trap (in-hole EC_2 (exit N)))) E S⊥
         (in-hole EC_1 (↓k (exit N))) E S⊥)]

  [(no-trap/par-context EC_2) (no-trap/par-context EC_3)
   --------------------------------------------------------------------------- "parexitL"
   (-->& (in-hole EC_1 (par (in-hole EC_2 (exit N)) (in-hole EC_3 pe_3))) E S⊥
         (in-hole EC_1 (Max-kk (exit N) pe_3)) E S⊥)]

  [(no-trap/par-context EC_2) (no-trap/par-context EC_3)
   --------------------------------------------------------------------------- "parexitR"
   (-->& (in-hole EC_1 (par (in-hole EC_2 pe_2) (in-hole EC_3 (exit N)))) E S⊥
         (in-hole EC_1 (Max-kk pe_2 (exit N))) E S⊥)]

  [---------------------------------- "trap done"
   (-->& (in-hole EC_1 (trap v)) E S⊥
         (in-hole EC_1 v) E S⊥)]


  [--------------------------------------- "if-tt"
   (-->& (in-hole EC (if tt e_1 e_2)) E S⊥
         (in-hole EC e_1) E S⊥)]

  [--------------------------------------- "if-f"
   (-->& (in-hole EC (if ff e_1 e_2)) E S⊥
         (in-hole EC e_2) E S⊥)]

  [-------------------------------------- "op"
   (-->& (in-hole EC (op v_1 v_2)) E S⊥
         (in-hole EC (δ op v_1 v_2)) E S⊥)]

  [(where s_2 ,(variable-not-in (term (EC e E)) (term s_1)))
   -------------------------------------------------------- "\\"
   (-->& (in-hole EC (e \\ s_1)) E S⊥
         (in-hole EC (rename e s_1 s_2)) (extend E s_2 ⊥) S⊥)]

  [(blocked e_1 E_1 S_1)
   (where #t (≠ S_1 ·))
   (emits e_1 E_1 S_1 S_2)
   (where #t (≠ S_2 ·)) ;; empty => nonconstructive
   --------------------------------------------- "can"
   ;; this rule doesn't seem general enough as we
   ;; might encounter a nested blocked configuration
   (-->& e_1 E_1 ⊥
         e_1 (extend-S E_1 S_2 ff) ⊥)])

(define-metafunction L
  rename : e s s -> e
  [(rename (! s_1) s_1 s_2) (! s_2)]
  [(rename (! s_3) s_1 s_2) (! s_3)]
  [(rename s_1 s_1 s_2) s_2]
  [(rename s_3 s_1 s_2) s_3]
  [(rename (s_1 ⊃ e) s_1 s_2) (s_2 ⊃ e)]
  [(rename (s_3 ⊃ e) s_1 s_2) (s_3 ⊃ e)]
  [(rename (seq e_1 e_2) s_1 s_2)
   (seq (rename e_1 s_1 s_2)
        (rename e_2 s_1 s_2))]
  [(rename (e *) s_1 s_2) ((rename e s_1 s_2) *)]
  [(rename (par e_1 e_2) s_1 s_2)
   (par (rename e_1 s_1 s_2)
        (rename e_2 s_1 s_2))]
  [(rename nothing s_1 s_2) nothing]
  [(rename pause s_1 s_2) pause]
  [(rename (trap e) s_1 s_2) (trap (rename e s_1 s_2))]
  [(rename (exit N) s_1 s_2) (exit N)]
  [(rename (e \\ s_1) s_1 s_2) (e \\ s_1)]
  [(rename (e \\ s_3) s_1 s_2) ((rename e s_1 s_2) \\ s_3)]
  [(rename (if e_1 e_2 e_3) s_1 s_2)
   (if (rename e_1 s_1 s_2)
       (rename e_2 s_1 s_2)
       (rename e_3 s_1 s_2))]
  [(rename B s_1 s_2) B]
  [(rename N s_1 s_2) N]
  [(rename (op e_1 e_2) s_1 s_2)
   (op (rename e_1 s_1 s_2)
       (rename e_2 s_1 s_2))])
(module+ test
  (test-equal (term (rename (seq (par (! x) x)
                                 (+ (x \\ x)
                                    (x \\ y)))
                            x z))
              (term (seq (par (! z) z)
                         (+ (x \\ x)
                            (z \\ y))))))

(define-judgment-form L
  #:contract (emits e E S S)
  #:mode (emits I I I O)

  [(emits e (extend E s tt) S_1 S_s=tt)
   (emits e (extend E s ff) S_1 S_s=ff)
   ------------------------------------- "fork"
   (emits e E (s S_1) (∪ S_s=tt S_s=ff))]

  [(-->&* e_1 E_1 · e_2 E_2 S)
   ------------------------------------ "run"
   (emits e_1 E_1 · (set- (dom E_1) S))])

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
  #:mode (no-trap/par-context I)
  #:contract (no-trap/par-context EC)
  
  [----
   (no-trap/par-context hole)]
  [(no-trap/par-context EC)
   ---
   (no-trap/par-context (s ⊃ EC))]
  [(no-trap/par-context EC)
   ---
   (no-trap/par-context (seq EC e))]
  [(no-trap/par-context EC)
   ---
   (no-trap/par-context (if EC e_1 e_2))]
  [(no-trap/par-context EC)
   ---
   (no-trap/par-context (op EC e))]
  [(no-trap/par-context EC)
   ---
   (no-trap/par-context (op v EC))])

(module+ test
  (test-judgment-holds
   (-->&* (if (< 1 2) (! O1) (! O2))
          ·
          ⊥
          nothing
          (O1 = tt ·)
          ⊥))

  (test-judgment-holds
   (-->&* (seq (trap (+ 2 (trap (if (exit 1) (! O1) (! O2))))) 3)
          ·
          ⊥
          3 · ⊥))

  (test-judgment-holds
   (-->&* (+ (trap (+ 2 3)) 4)
          · ⊥
          9 · ⊥))

  (test-judgment-holds
   (-->&* (pause *)
          · ⊥
          (seq pause (pause *))
          · ⊥))

  (test-judgment-holds
   (blocked (if s1 (! O1) (! O2)) (extend · s1 ⊥) (s1 ·)))

  (test-judgment-holds
   (emits (if s1 (! O1) (! O2))
          (extend · s1 ⊥)
          (s1 ·)
          (s1 ·)))

  (test-judgment-holds
   (-->& (if s1 (! O1) (! O2))
         (extend · s1 ⊥)
         ⊥
         (if s1 (! O1) (! O2))
         (s1 = ff ·)
         ⊥))

  (test-judgment-holds
   (-->&* (if s1 (! O1) (! O2))
          (extend · s1 ⊥)
          ⊥
          nothing
          (s1 = ff (O2 = tt ·))
          ⊥))

  (test-equal
   (judgment-holds
    (-->&* (trap
            (seq (trap (par (exit 0)
                            (exit 1)))
                 (! O1)))
           (extend · O1 ⊥)
           ⊥
           nothing
           (O1 = tt ·)
           ⊥))
   #f)

  (test-judgment-holds
   (-->&* (seq (trap
                (seq (trap (par (exit 0)
                                (exit 1)))
                     (! O1)))
               (! O2))
          (extend (extend · O2 ⊥) O1 ⊥)
          ⊥
          nothing
          (O2 = tt (O1 = ⊥ ·))
          ⊥))

  (test-judgment-holds
   (emits (par (if s1 (! s1) (! O1))
               (if s2 (! O2) (! s1)))
          (s2 = ⊥ (s1 = ⊥ ·))
          (s2 (s1 ·))
          S)))
