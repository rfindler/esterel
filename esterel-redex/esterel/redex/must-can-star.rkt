#lang racket
(require redex/reduction-semantics
         "lang.rkt" "helpers.rkt")
(provide mc*)

(define-judgment-form L
  #:contract (mc* fn e E* R*)
  #:mode (mc* I I I O)
  [---- "k"
   (mc* fn k* E* (Pr (set) (set k*)))]

  [(mc* fn e E* (Pr S* K*))
   ---- "!"
   (mc* fn (! s e) E* (Pr (s = K* ·) (set nothing)))]

  [(lookup*-B⊥ E* s tt)
   ---- "s tt"
   (mc* fn s E* (Pr (set) (set tt)))]
  
  [(lookup*-B⊥ E* s ff)
   ---- "s ff"
   (mc* fn s E* (Pr (set) (set ff)))]

  [(lookup*-B⊥ E* s ⊥)
   ---- "s Must ⊥"
   (mc* Must s E* (Pr (set) (set)))]

  [(lookup*-B⊥ E* s ⊥)
   ---- "s Can ⊥"
   (mc* Can s E* (Pr (set) (set tt ff)))]

  [(mc* fn e E* R*)
   ---- "⊃"
   (mc* fn (s ⊃ e) E* R*)]

  [(mc* fn e_1 E* (Pr S*_p K*_1)) (∉ nothing K*_1)
   ---- "; 0 ∉ p"
   (mc* fn (seq e_1 e_2) E* (Pr S*_p K*_1))]

  [(mc* Must e_1 E* (Pr S*_1 K*_1)) (∈ nothing K*_1) (mc* Must e_2 E* (Pr S*_2 K*_2))
   ---- "Must ; 0 ∈ p"
   (mc* Must (seq e_1 e_2) E* (Pr (merge-S* S*_1 S*_2) (∪ (parens (set- K*_1 nothing)) K*_2)))]

  [(mc* Can e_1 E* (Pr S*_1 K*_1)) (∈ nothing K*_1) (mc* Must e_2 E* (Pr S*_mustp K*_must1))
   (where fn_2 (pickfn-seq Can K*_must1)) (mc* fn_2 e_2 E* (Pr S*_2 K*_2))
   ---- "Can ; 0 ∈ p"
   (mc* Can (seq e_1 e_2) E* (Pr (merge-S* S*_1 S*_2) (∪ (set- K*_1 nothing) K*_2)))]

  [(mc* fn e E* R*)
   ---- "*"
   (mc* fn (e *) E* R*)]

  [(mc* fn e_1 E* (Pr S*_1 K*_1)) (mc* fn e_2 E* (Pr S*_2 K*_2))
   ---- "par"
   (mc* fn (par e_1 e_2) E* (Pr (merge-S* S*_1 S*_2) (Max K*_1 K*_2)))]

  [(mc* fn e E* (Pr S* K*))
   ---- "trap"
   (mc* fn (trap e) E* (Pr S* (↓ K*)))]

  [(mc* Must e (extend* E* s ⊥ new 0) (Pr S*_⊥ K*_⊥)) (∈ s (dom S*_⊥)) (lookup-S* S*_⊥ s (N ·))
   (mc* fn+ e (extend* E* s tt ready N) (Pr S* K*))
   ---- "fn+\\tt"
   (mc* fn+ (e \\ s) E* (Pr (remove-from-dom S* s) K*))]

  [(mc* Can+ e (extend* E* s ⊥ new 0) (Pr S*_⊥ K*_⊥)) (∉ s (dom S*_⊥))
   (mc* fn+ e (extend* E* s ff ready 0) (Pr S* K*))
   ---- "fn+\\ff"
   (mc* fn+ (e \\ s) E* (Pr (remove-from-dom S* s) K*))]

  [(mc* Must e (extend* E* s ⊥ new 0) (Pr S*_m⊥ K*_m⊥))
   (mc* Can+ e (extend* E* s ⊥ new 0) (Pr S*_c⊥ K*_c⊥))
   (∉ s (dom S*_m⊥)) (∈ s (dom S*_c⊥)) (mc* fn+ e (extend* E* s ⊥ new 0) (Pr S* K*))
   ---- "fn+\\⊥"
   (mc* fn+ (e \\ s) E* (Pr (remove-from-dom S* s) K*))]

  [(mc* Can⊥ e (extend* E* s ⊥ new 0) (Pr S*_⊥ K*_⊥)) (∉ s (dom S*_⊥))
   (mc* Can⊥ e (extend* E* s ff new 0) (Pr S* K*))
   ---- "Can⊥\\ff"
   (mc* Can⊥ (e \\ s) E* (Pr (remove-from-dom S* s) K*))]

  [(mc* Can⊥ e (extend* E* s ⊥ new 0) (Pr S* K*)) (∈ s (dom S*))
   ---- "Can⊥\\⊥"
   (mc* Can⊥ (e \\ s) E* (Pr (remove-from-dom S* s) K*))]

  [(mc* fn e_1 E* (Pr S*_1 K*_1)) (∈ tt K*_1) (∉ ff K*_1) (mc* fn e_2 E* (Pr S*_2 K*_2))
   ---- "if tt"
   (mc* fn (if e_1 e_2 e_3) E* (Pr (merge-S* S*_1 S*_2) K*_2))]

  [(mc* fn e_1 E* (Pr S*_1 K*_1)) (∉ tt K*_1) (∈ ff K*_1) (mc* fn e_3 E* (Pr S*_3 K*_3))
   ---- "if ff"
   (mc* fn (if e_1 e_2 e_3) E* (Pr (merge-S* S*_1 S*_3) K*_3))]

  [(mc* fn e_1 E* (Pr S*_1 K*_1)) (∉ tt K*_1) (∉ ff K*_1)
   ---- "if neither"
   (mc* fn (if e_1 e_2 e_3) E* (Pr S*_1 (set)))]

  [(mc* fn e_1 E* (Pr S*_1 K*_1)) (∈ tt K*_1) (∈ ff K*_1)
   (mc* fn e_2 E* (Pr S*_2 K*_2)) (mc* fn e_3 E* (Pr S*_3 K*_3))
   ---- "if both"
   (mc* fn (if e_1 e_2 e_3) E* (Pr (merge-S* S*_1 (merge-S* S*_2 S*_3)) (∪ K*_2 K*_3)))]

  [(mc* fn e_1 E* (Pr S*_1 K*_1)) (mc* fn e_2 E* (Pr S*_2 K*_2))
   ---- "op"
   (mc* fn (op e_1 e_2) E* (Pr (merge-S* S*_1 S*_2) (op-each-pair op K*_1 K*_2)))]
  )

(module+ test
  (test-judgment-holds
   (mc* Must (+ 1 2) · (Pr · (3 ·))))
  (test-judgment-holds
   (mc* Can+
        (+ (if S 2 4) (if S 10 30))
        (extend* · S ⊥ new 0)
        (Pr · (12 (32 (14 (34 ·))))))))

(define-metafunction L
  pickfn-seq : Can set -> Can
  [(pickfn-seq Can+ set)
   Can+
   (judgment-holds (∈ nothing set))]
  [(pickfn-seq Can set) Can⊥])


(define-metafunction L
  p-to-e : p -> e
  [(p-to-e (! s)) (! s 0)]
  [(p-to-e (? s p q)) (if s (p-to-e p) (p-to-e q))]
  [(p-to-e (s ⊃ p)) (s ⊃ (p-to-e p))]
  [(p-to-e (seq p q)) (seq (p-to-e p) (p-to-e q))]
  [(p-to-e (p *)) ((p-to-e p) *)]
  [(p-to-e (par p q)) (par (p-to-e p) (p-to-e q))]
  [(p-to-e (trap p)) (trap (p-to-e p))]
  [(p-to-e nothing) nothing]
  [(p-to-e pause) pause]
  [(p-to-e (exit N)) (exit N)]
  [(p-to-e (p \\ s)) ((p-to-e p) \\ s)])

(define-metafunction L
  R*->R : R* -> R
  [(R*->R (Pr S* K*)) (Pr (dom S*) K*)])

(module+ test
  (require "must-can.rkt")

  (define (call-mc fn p)
    (judgment-holds (mc ,fn ,p (⊥E (close ,p)) R) R))
  (define (call-mc* fn p)
    (judgment-holds (mc* ,fn (p-to-e ,p) (⊥E* (close ,p)) R*) (R*->R R*)))
  (define (mc-same? fn p)
    (equal? (call-mc fn p)
            (call-mc* fn p)))
  
  (redex-check
   L (fn p) #:ad-hoc
   (mc-same? (term fn) (term p))))

(module+ main
  (require redex/gui)

  (define (remove-from-derivations to-remove derivations)
    (let loop ([derivations derivations])
      (for/list ([a-derivation (in-list derivations)]
                 #:unless
                 (member (car (derivation-term a-derivation)) to-remove))
        (derivation (derivation-term a-derivation)
                    (derivation-name a-derivation)
                    (loop (derivation-subs a-derivation))))))

  (show-derivations
   (remove-from-derivations
    '(lookup lookup*-value ∈ ∉)
    (build-derivations
     (mc* Can+
          (((par (if S1 (! S2) nothing)
                 (if S2 nothing nothing))
            \\ S2) \\ S1)
          (O1 = ⊥ new 0 (O2 = ⊥ new 0 ·))
          R*)))))
