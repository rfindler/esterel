#lang racket
(require redex/reduction-semantics)
(define-language L
  (p q ::=
     (! s) (? s p q) (s ⊃ p)
     (seq p q) (p *) (par p q)
     (trap p) k
     (p \\ s))
  (k ::= natural)
  (s ::= variable-not-otherwise-mentioned)

  (fn ::= Must Can)
  (Can ::= Can+ Can⊥)
  (E ::= · (s = B⊥ E))
  (B⊥ ::= tt ff ⊥)
  (B ::= tt ff)
  (set ::= · (any set))
  (S ::= · (s S))
  (K ::= · (k K))
  (R ::= (Pr S K)))

(define-metafunction L
  mc-s : fn p E -> S
  [(mc-s fn p E) S (where (Pr S K) (mc fn p E))])

(define-metafunction L
  mc-k : fn p E -> K
  [(mc-k fn p E) K (where (Pr S K) (mc fn p E))])

(define-metafunction L
  mc : fn p E -> R
  [(mc fn k E) (Pr · (k ·))]
  
  [(mc fn (! s) E) (Pr (s ·) (0 ·))]
  
  [(mc fn (? s p q) E R)
   (mc fn p E R)
   (where tt (lookup s E))]
  
  [(mc fn (? s p q) E R)
   (mc fn q E R)
   (where ff (lookup s E))]

  [(mc Must (? s p q) E)
   (Pr · ·)]

  [(mc Can (? s p q) E)
   (Pr (∪ (mc-s Must p E)
          (mc-s Must q E))
       (∪ (mc-k Must p E)
          (mc-k Must q E)))]

  [(mc fn (s ⊃ p))
   (mc fn p)]

  [(mc fn (seq p q) E)
   (mc fn p E)
   (where #false (∈ 0 (mc-k fn p E)))]

  [(mc fn (seq p q) E)
   (Pr (∪ (mc-s fn p E) (mc-s fn_2 q E))
       (∪ (set- (mc-k fn p E) 0)
          (mc-k fn_2 q E)))
   (where fn_2 (pickfn-seq fn p E))]

  [(mc fn (p *) E) (mc fn p E)]

  [(mc fn (par p q) E)
   (Pr (∪ (mc-s fn p E) (mc-s fn q E))
       (Max (mc-k fn p E) (mc-k fn q E)))]

  [(mc fn (trap p) E)
   (Pr (mc-s fn p E)
       (↓ (mc-k fn p E)))]

  [(mc Must (p \\ s) E)
   (remove-s (mc Must p (extend s ff E)) s)
   (where #true (∈ s (mc-s Must p (extend s ⊥ E))))]

  [(mc Must (p \\ s) E)
   (remove-s (mc Must p (extend s ff E)) s)
   (where #false (∈ s (mc-s Can+ p (extend s ⊥ E))))]

  [(mc Must (p \\ s) E)
   (remove-s (mc Must p (extend s ⊥ E)) s)]

  [(mc Can+ (p \\ s) E)
   (remove-s (mc Can+ p (extend s tt E)) s)
   (where #true (∈ s (mc-s Must p (extend s ⊥ E))))]

  [(mc Can (p \\ s) E)
   (remove-s (mc Can p (extend s ff E)) s)
   (where #false (∈ s (mc-s Can p (extend s ⊥ E))))]

  [(mc Can (p \\ s) E)
   (remove-s (mc Can p (extend s ⊥ E)) s)]

  )

(define-judgment-form L
  mc : fn p E -> R
  [(mc fn k E) (Pr · (k ·))]
  
  [(mc fn (! s) E) (Pr (s ·) (0 ·))]
  
  [(mc fn (? s p q) E R)
   (mc fn p E R)
   (where tt (lookup s E))]
  
  [(mc fn (? s p q) E R)
   (mc fn q E R)
   (where ff (lookup s E))]

  [(mc Must (? s p q) E)
   (Pr · ·)]

  [(mc Can (? s p q) E)
   (Pr (∪ (mc-s Can p E)
          (mc-s Can q E))
       (∪ (mc-k Can p E)
          (mc-k Can q E)))]

  [(mc fn (s ⊃ p))
   (mc fn p)]

  [(mc fn (seq p q) E)
   (mc fn p E)
   (where #false (∈ 0 (mc-k fn p E)))]

  [(mc fn (seq p q) E)
   (Pr (∪ (mc-s fn p E) (mc-s fn_2 q E))
       (∪ (set- (mc-k fn p E) 0)
          (mc-k fn_2 q E)))
   (where fn_2 (pickfn-seq fn p E))]

  [(mc fn (p *) E) (mc fn p E)]

  [(mc fn (par p q) E)
   (Pr (∪ (mc-s fn p E) (mc-s fn q E))
       (Max (mc-k fn p E) (mc-k fn q E)))]

  [(mc fn (trap p) E)
   (Pr (mc-s fn p E)
       (↓ (mc-k fn p E)))]

  [(mc Must (p \\ s) E)
   (remove-s (mc Must p (extend s ff E)) s)
   (where #true (∈ s (mc-s Must p (extend s ⊥ E))))]

  [(mc Must (p \\ s) E)
   (remove-s (mc Must p (extend s ff E)) s)
   (where #false (∈ s (mc-s Can+ p (extend s ⊥ E))))]

  [(mc Must (p \\ s) E)
   (remove-s (mc Must p (extend s ⊥ E)) s)]

  [(mc Can+ (p \\ s) E)
   (remove-s (mc Can+ p (extend s tt E)) s)
   (where #true (∈ s (mc-s Must p (extend s ⊥ E))))]

  [(mc Can (p \\ s) E)
   (remove-s (mc Can p (extend s ff E)) s)
   (where #false (∈ s (mc-s Can p (extend s ⊥ E))))]

  [(mc Can (p \\ s) E)
   (remove-s (mc Can p (extend s ⊥ E)) s)]

  )



(define-metafunction L
  Max : K K -> K
  [(Max · K) ·]
  [(Max K ·) ·]
  [(Max (k K_1) K_2)
   (∪ (Max-kK k K_2)
      (Max K_1 K_2))])

(define-metafunction L
  Max-kK : k K -> K
  [(Max-kK k ·) ·]
  [(Max-kK k_1 (k_2 K)) (∪ (Max-kK k_1 K) ((Max-kk k_1 k_2) ·))])

(define-metafunction L
  Max-kk : k k -> k
  [(Max-kk k_1 k_2) ,(max (term k_1) (term k_2))])

(module+ test
  (test-equal (term (Max · ·)) (term ·))
  (test-equal (term (Max · (1 ·))) (term ·))
  (test-equal (term (Max (1 ·) ·)) (term ·))
  (test-equal (term (Max (1 ·) (2 ·))) (term (2 ·)))
  (test-equal (term (Max (1 (2 (3 ·))) (2 (3 (4 ·))))) (term (4 (3 (2 ·)))))
  (test-equal (term (Max (0 (2 (4 (6 ·)))) (1 (3 (5 ·))))) (term (5 (3 (1 (2 (4 (6 ·)))))))))

(define-metafunction L
  ↓ : K -> K
  [(↓ ·) ·]
  [(↓ (k K)) ((↓k k) (↓ K))])

(define-metafunction L
  ↓k : k -> k
  [(↓k 0) 0]
  [(↓k 2) 0]
  [(↓k 1) 1]
  [(↓k k) ,(- (term k) 1)])

(module+ test
  (test-equal (term (↓ (0 (1 (2 (3 (4 ·)))))))
              (term (0 (1 (0 (2 (3 ·))))))))

(define-metafunction L
  pickfn-seq : fn p E -> fn
  [(pickfn-seq Must p E) Must]
  [(pickfn-seq Can+ p E)
   Can+
   (where #true (∈ 0 (mc-k Must p E)))]
  [(pickfn-seq fn p E) Can⊥])

(define-metafunction L  
  ∪ : set set -> set
  [(∪ · set) set]
  [(∪ (any set_1) set_2) (any (∪ set_1 (set- set_2 any)))])

(define-metafunction L
  set- : set any -> set
  [(set- · any) ·]
  [(set- (any set) any) set]
  [(set- (any_1 set) any_2) (any_1 (set- set any_2))])

(define-metafunction L
  ∈ : any set -> boolean
  [(∈ any ·) #false]
  [(∈ any (any set)) #true]
  [(∈ any_1 (any_2 set)) (∈ any_1 set)])

(module+ test
  (test-equal (term (set- · 1)) (term ·))
  (test-equal (term (set- (1 ·) 1)) (term ·))
  (test-equal (term (set- (2 (1 ·)) 1)) (term (2 ·)))
  (test-equal (term (set- (1 (2 ·)) 1)) (term (2 ·)))
  (test-equal (term (∪ · ·)) (term ·))
  (test-equal (term (∪ (1 ·) ·)) (term (1 ·)))
  (test-equal (term (∪ · (1 ·))) (term (1 ·)))
  (test-equal (term (∪ (1 ·) (1 ·))) (term (1 ·)))
  (test-equal (term (∪ (2 ·) (1 ·))) (term (2 (1 ·))))
  (test-equal (term (∪ (1 ·) (2 ·))) (term (1 (2 ·))))
  (test-equal (term (∪ (3 (2 (1 ·))) (1 (2 (3 ·))))) (term (3 (2 (1 ·)))))
  (test-equal (term (∈ 1 (3 (2 (1 ·))))) #true)
  (test-equal (term (∈ 2 (3 (2 (1 ·))))) #true)
  (test-equal (term (∈ 3 (3 (2 (1 ·))))) #true)
  (test-equal (term (∈ 4 (3 (2 (1 ·))))) #false))

(define-metafunction L
  remove-s : R s -> R
  [(remove-s (Pr S K) s) (Pr (set- S s) K)])

(define-metafunction L
  lookup : s E -> B⊥
  [(lookup s (s = B⊥ E))
   B⊥]
  [(lookup s_1 (s_2 = B⊥ E))
   (lookup s_1 E)])

(define-metafunction L
  extend : s B⊥ E -> E
  [(extend s B⊥ E) (s = B⊥ E)])

(module+ test
  (test-equal (term (lookup s1 (extend s1 tt (extend s2 ff (extend s3 ⊥ ·)))))
              (term tt))
  (test-equal (term (lookup s2 (extend s1 tt (extend s2 ff (extend s3 ⊥ ·)))))
              (term ff))
  (test-equal (term (lookup s3 (extend s1 tt (extend s2 ff (extend s3 ⊥ ·)))))
              (term ⊥)))


(module+ test
  (test-equal (term (mc Must ((? s (! s) (! s)) \\ s) ·)) (term (Pr · ·)))
  (test-equal (term (mc Can+ ((? s (! s) (! s)) \\ s) ·)) (term (Pr · (0 ·))))
  (test-equal (term (mc Can⊥ ((? s (! s) (! s)) \\ s) ·)) (term (Pr · (0 ·))))
  (test-equal (term (mc Must (? s (! s) (! s)) ·)) (term (Pr · ·)))
  (test-equal (term (mc Can+ (? s (! s) (! s)) ·)) (term (Pr (s ·) (0 ·))))
  (test-equal (term (mc Can⊥ (? s (! s) (! s)) ·)) (term (Pr (s ·) (0 ·))))
  (test-equal (term (mc Must (par (! s) (! t)) ·)) (term (Pr (s (t ·)) (0 ·))))
  (test-equal (term (mc Can+ (par (! s) (! t)) ·)) (term (Pr (s (t ·)) (0 ·))))
  (test-equal (term (mc Can⊥ (par (! s) (! t)) ·)) (term (Pr (s (t ·)) (0 ·))))

  (test-equal (term (mc Must (seq 1 2) ·)) (term (Pr · (1 ·))))
  (test-equal (term (mc Can+ (seq 1 2) ·)) (term (Pr · (1 ·))))
  (test-equal (term (mc Can⊥ (seq 1 2) ·)) (term (Pr · (1 ·))))
  (test-equal (term (mc Must (seq 0 1) ·)) (term (Pr · (1 ·))))
  (test-equal (term (mc Can+ (seq 0 1) ·)) (term (Pr · (1 ·))))
  (test-equal (term (mc Can⊥ (seq 0 1) ·)) (term (Pr · (1 ·))))
  )

(current-traced-metafunctions '(mc))
(define P8
  (term
   (seq (trap
         (par
          (seq (? I 0 1)
               (! O))
          (? O 0 2)))
        (! O))))
(term (mc Can+ ,P8 ·))
(term (mc Can⊥ ,P8 ·))

