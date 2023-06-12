#lang racket
(require redex/reduction-semantics
         "helpers.rkt" "lang.rkt"
         "must-can-hat.rkt"
         "red-and.rkt")

(provide eval eval&)

(define-judgment-form L
  #:mode (eval I I O)
  #:contract (eval e E E+nc)

  [(set= (dom E_in) (fv-e e))
   --------------------
   (eval e E_in E_in)]

  [(set≠ (dom E_in) (fv-e e))
   (mc^ Can+ e E_in (Pr S_can+ K^_can+))
   (mc^ Must e E_in (Pr S_must K^_must))
   (set≠ (dom (extend-S (extend-S E_in (set- (fv-e e) S_can+) ff) S_must tt))
         (dom E_in))
   (eval e (extend-S (extend-S E_in (set- (fv-e e) S_can+) ff) S_must tt) E_out)
   --------------------
   (eval e E_in E_out)]

  [(set≠ (dom E_in) (fv-e e))
   (mc^ Can+ e E_in (Pr S_can+ K^_can+))
   (mc^ Must e E_in (Pr S_must K^_must))
   (set= (dom (extend-S (extend-S E_in (set- (fv-e e) S_can+) ff) S_must tt))
         (dom E_in))
   --------------------
   (eval e E_in nonconstructive)])

(module+ test
  (test-judgment-holds
   (eval ((if s (! O1) (! O2)) \\ s)
         ·
         (O1 = ff (O2 = tt ·))))

  (test-judgment-holds
   (eval ((if s (! s) (! O2)) \\ s)
         ·
         nonconstructive))

  (test-judgment-holds
   (eval ((if s (! O1) (! s)) \\ s)
         ·
         nonconstructive)))

(define-judgment-form L
  #:mode (eval& I I O)
  #:contract (eval& e E E+nc)

  [(-->&* e_1 E_in ⊥ e_2 E_out ⊥)
   ------------------------------ "constructive"
   (eval& e_1 E_in E_out)]


  [(-->&* e_1 E_in ⊥ e_2 E_out ⊥)
   (blocked e_2 E_in S)
   (where #t (≠ S ·))
   (emits e_2 E_in S ·)
   -------------------------------- "nonconstructive"
   (eval& e_1 E_in nonconstructive)])

(module+ test

  ;; this test case is kind of suspect,
  ;; because I'm "guessing" that the fresh
  ;; name is `s1` but maybe s1 shouldn't
  ;; even be in there at all
  (test-judgment-holds
   (eval& ((if s (! O1) (! O2)) \\ s)
          ·
          (s1 = ff (O2 = tt ·))))

  (test-judgment-holds
   (eval ((if s (! s) (! O2)) \\ s)
         ·
         nonconstructive))

  (test-judgment-holds
   (eval ((if s (! O1) (! s)) \\ s)
         ·
         nonconstructive)))
