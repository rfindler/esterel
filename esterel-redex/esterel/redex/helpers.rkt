#lang racket
(require redex/reduction-semantics "lang.rkt")
(provide Max ↓ ↓k Max-kk Max-k&k&
         lookup extend extend-S
         remove dom
         ∈ ∉ ∪ set- set set= set≠
         ⊥E S->E fv-p fv-e
         op-each-pair δ
         parens ≠)

(define-metafunction L
  fv-p : p -> S
  [(fv-p (! s)) (s ·)]
  [(fv-p (? s p q)) (∪ (s ·) (∪ (fv-p p) (fv-p q)))]
  [(fv-p (s ⊃ p)) (∪ (s ·) (fv-p p))]
  [(fv-p (seq p q)) (∪ (fv-p p) (fv-p q))]
  [(fv-p (p *)) (fv-p p)]
  [(fv-p (par p q)) (∪ (fv-p p) (fv-p q))]
  [(fv-p (trap p)) (fv-p p)]
  [(fv-p nothing) ·]
  [(fv-p pause) ·]
  [(fv-p (exit N)) ·]
  [(fv-p (p \\ s)) (set- (fv-p p) (set s))])

(module+ test
  (test-equal (term (fv-p (? s (! t) (trap (w ⊃ ((! z) *))))))
              (term (s (t (w (z ·))))))
  (test-equal (term (fv-p ((! t) \\ t)))
              (term ·)))

(define-metafunction L
  fv-e : e -> S
  [(fv-e (! s)) (s ·)]
  [(fv-e s) (s ·)]
  [(fv-e (if e_1 e_2 e_3)) (∪ (fv-e e_1) (∪ (fv-e e_2) (fv-e e_3)))]
  [(fv-e (s ⊃ e)) (∪ (s ·) (fv-e e))]
  [(fv-e (seq e_1 e_2)) (∪ (fv-e e_1) (fv-e e_2))]
  [(fv-e (e *)) (fv-e e)]
  [(fv-e (par e_1 e_2)) (∪ (fv-e e_1) (fv-e e_2))]
  [(fv-e (trap e)) (fv-e e)]
  [(fv-e nothing) ·]
  [(fv-e pause) ·]
  [(fv-e (exit N)) ·]
  [(fv-e (e \\ s)) (set- (fv-e e) (set s))])

(module+ test
  (test-equal (term (fv-e (if s (! t) (trap (w ⊃ ((! z) *))))))
              (term (s (t (w (z ·))))))
  (test-equal (term (fv-e ((! t) \\ t)))
              (term ·)))

(define-metafunction L
  ⊥E : S -> E
  [(⊥E ·) ·]
  [(⊥E (s S)) (s = ⊥ (⊥E S))])

(define-metafunction L
  S->E : B S -> E
  [(S->E B ·) ·]
  [(S->E B (s S)) (s = B (S->E B S))])

(define-metafunction L
  op-each-pair : op K^ K^ -> K^
  [(op-each-pair op · K^) ·]
  [(op-each-pair op (k^ K^_1) K^_2)
   (∪ (op-each op k^ K^_2) (op-each-pair op K^_1 K^_2))])

(define-metafunction L
  op-each : op k^ K^ -> K^
  [(op-each op k^ ·) ·]
  [(op-each op k^_1 (k^_2 K^))
   (∪ (set (δ op k^_1 k^_2)) (op-each op k^_1 K^))])

(define-metafunction L
  δ : op k^ k^ -> k^
  [(δ op (exit N_1) (exit N_2)) (exit ,(max (term N_1) (term N_2)))]
  [(δ op pause (exit N)) (exit N)]
  [(δ op (exit N) pause) (exit N)]
  [(δ op pause k^) pause]
  [(δ op k^ pause) pause]
  [(δ + N_1 N_2) ,(+ (term N_1) (term N_2))]
  [(δ - N_1 N_2) ,(- (term N_1) (term N_2))]
  [(δ < N_1 N_2) ,(if (< (term N_1) (term N_2)) (term tt) (term ff))]
  [(δ = N_1 N_1) tt]
  [(δ = N_1 N_2) ff])

(module+ test
  (test-equal (term (δ + 1 2)) (term 3))
  (test-equal (term (δ - 2 1)) (term 1))
  (test-equal (term (δ < 2 1)) (term ff))
  (test-equal (term (δ < 1 2)) (term tt))
  (test-equal (term (δ = 1 2)) (term ff))
  (test-equal (term (δ = 1 1)) (term tt))

  (test-equal (term (op-each-pair + (set 1) (set 2)))
              (term (set 3)))
  (test-equal (term (op-each-pair - (set 5 4 3) (set 2 1)))
              (term (set 3 4 2 1))))

(define-metafunction L
  Max : K^ K^ -> K^
  [(Max · K) ·]
  [(Max (k K_1) K_2)
   (∪ (Max-kK k K_2)
      (Max K_1 K_2))])

(define-metafunction L
  Max-kK : k^ K^ -> K^
  [(Max-kK k^ ·) ·]
  [(Max-kK k^_1 (k^_2 K^)) (∪ (Max-kK k^_1 K^) ((Max-kk k^_1 k^_2) ·))])

(define-metafunction L
  Max-kk : k^ k^ -> k^
  [(Max-kk (exit N_1) (exit N_2)) (exit ,(max (term N_1) (term N_2)))]
  [(Max-kk (exit N) k^) (exit N)]
  [(Max-kk k^ (exit N)) (exit N)]
  [(Max-kk k^ pause) pause]
  [(Max-kk pause k^) pause]

  ;; just declare that `par` discards the results of
  ;; the branches and always returns `nothing`
  [(Max-kk nothing k^) nothing]
  [(Max-kk k^ nothing) nothing])

(define-metafunction L
  Max-k&k& : k& k& -> k&
  [(Max-k&k& (blocked S_1) (blocked S_2)) (blocked (∪ S_1 S_2))]
  [(Max-k&k& (blocked S) k^) (blocked S)]
  [(Max-k&k& k^ (blocked S)) (blocked S)]
  [(Max-k&k& k^_1 k^_2) (Max-kk k^_1 k^_2)])

(module+ test
  (test-equal (term (Max-kk pause pause)) (term pause))
  (test-equal (term (Max · ·)) (term ·))
  (test-equal (term (Max · (pause ·))) (term ·))
  (test-equal (term (Max (pause ·) ·)) (term ·))
  (test-equal (term (Max (pause ·) ((exit 0) ·))) (term ((exit 0) ·)))
  (test-equal (term (Max ((exit 0) ·) (pause ·))) (term ((exit 0) ·)))
  (test-equal (term (Max (pause ((exit 0) ((exit 1) ·))) ((exit 0) ((exit 1) ((exit 2) ·)))))
              (term ((exit 2) ((exit 1) ((exit 0) ·)))))
  (test-equal (term (Max ((exit 0) ((exit 1) ((exit 2) ·))) (pause ((exit 0) ((exit 1) ·)))))
              (term ((exit 1) ((exit 0) ((exit 2) ·)))))
  (test-equal (term (Max (nothing ((exit 0) ((exit 2) ((exit 4) ·)))) (pause ((exit 1) ((exit 3) ·)))))
              (term ((exit 3) ((exit 1) (pause ((exit 0) ((exit 2) ((exit 4) ·))))))))
  (test-equal (term (Max (pause ((exit 1) ((exit 3) ·))) (nothing ((exit 0) ((exit 2) ((exit 4) ·))))))
              (term ((exit 4) ((exit 2) ((exit 0) (pause ((exit 1) ((exit 3) ·)))))))))

(define-metafunction L
  ↓ : K -> K
  [(↓ ·) ·]
  [(↓ (k K)) (∪ ((↓k k) ·) (↓ K))])

(define-metafunction L
  ↓k : k -> k
  [(↓k nothing) nothing]
  [(↓k (exit 0)) nothing]
  [(↓k pause) pause]
  [(↓k (exit N)) (exit ,(- (term N) 1))])

(module+ test
  (test-equal (term (↓ ((exit 11) ((exit 12) ((exit 15) ·)))))
              (term ((exit 10) ((exit 11) ((exit 14) ·)))))
  (test-equal (term (↓ (nothing ((exit 0) ·))))
              (term (nothing ·)))
  (test-equal (term (↓ (nothing (pause ·))))
              (term (nothing (pause ·))))
  (test-equal (term (↓ (pause ((exit 0) ·))))
              (term (pause (nothing ·))))
  (test-equal (term (↓ (nothing (pause ((exit 0) ·)))))
              (term (nothing (pause ·))))
  (test-equal (term (↓ (nothing (pause ((exit 0) ((exit 1) ((exit 2) ·)))))))
              (term (nothing (pause ((exit 0) ((exit 1) ·)))))))

(define-metafunction L
  ∪ : set set -> set
  [(∪ · set) set]
  [(∪ (any set_1) set_2) (any (∪ set_1 (set-1 set_2 any)))])
(module+ test
  (test-equal (term (∪ (a (b ·)) (c (d ·))))
              (term (a (b (c (d ·)))))))

(define-metafunction L
  set-1 : set any -> set
  [(set-1 · any) ·]
  [(set-1 (any set) any) set]
  [(set-1 (any_1 set) any_2) (any_1 (set-1 set any_2))])

(define-metafunction L
  set- : set set -> set
  [(set- set ·) set]
  [(set- set_1 (any set_2)) (set- (set-1 set_1 any) set_2)])

(define-judgment-form L
  #:mode (set= I I)
  #:contract (set= S S)
  [(where #t (subset S_1 S_2))
   (where #t (subset S_2 S_1))
   --------------
   (set= S_1 S_2)])

(module+ test
  (test-equal (judgment-holds (set= (a (b (c ·))) (a (d (b (c (e ·))))))) #f)
  (test-judgment-holds (set= (a (b (c ·))) (c (b (a ·))))))

(define-metafunction L
  subset : S S -> boolean
  [(subset · S) #t]
  [(subset (s S_1) S_2)
   (subset S_1 S_2)
   (where #t (∈ s S_2))]
  [(subset S_1 S_2) #f])

(module+ test
  (test-equal (term (subset (a (b (c ·))) (a (d (b (c (e ·))))))) #t)
  (test-equal (term (subset (a (d (b (c (e ·))))) (a (b (c ·))))) #f))

(define-judgment-form L
  #:mode (set≠ I I)
  #:contract (set≠ S S)
  [(where #f (subset S_1 S_2))
   --------------
   (set≠ S_1 S_2)]

  [(where #f (subset S_2 S_1))
   --------------
   (set≠ S_1 S_2)])

(module+ test
  (test-equal (judgment-holds (set≠ · ·)) #f)
  (test-equal (judgment-holds (set≠ (a (b (c ·))) (c (b (a ·))))) #f)
  (test-equal (judgment-holds (set≠ (a (b (c ·))) (a (b (c ·))))) #f)
  (test-judgment-holds (set≠ (a (b (c ·))) (a (b (c (d ·))))))
  (test-judgment-holds (set≠ (a (b (c (d ·)))) (a (b (c ·))))))

(define-judgment-form L
  #:mode (∈ I I)
  #:contract (∈ any set)
  [---------------------
   (∈ any_1 (any_1 set))]

  [(where #true (≠ any_1 any_2))
   (∈ any_1 set)
   ---------------------
   (∈ any_1 (any_2 set))])

(define-metafunction L
  set : any ... -> set
  [(set) ·]
  [(set any_1 any_2 ...) (any_1 (set any_2 ...))])
  

(define-judgment-form L
  #:mode (∉ I I)
  #:contract (∉ any set)
  [---------------------
   (∉ any_1 ·)]

  [(where #true (≠ any_1 any_2))
   (∉ any_1 set)
   ---------------------
   (∉ any_1 (any_2 set))])

(define-metafunction L
  ≠ : any any -> boolean
  [(≠ any any) #false]
  [(≠ any_1 any_2) #true])

(module+ test
  (test-judgment-holds (∈ 1 (2 (1 (3 ·)))))
  (test-judgment-holds (∉ 4 (2 (1 (3 ·)))))
  (test-equal (judgment-holds (∈ 1 (2 (3 ·)))) #false)
  (test-equal (judgment-holds (∉ 3 (2 (3 ·)))) #false))

(module+ test
  (test-equal (term (set- · (1 ·))) (term ·))
  (test-equal (term (set- (1 ·) (1 ·))) (term ·))
  (test-equal (term (set- (2 (1 ·)) (1 ·))) (term (2 ·)))
  (test-equal (term (set- (1 (2 ·)) (1 ·))) (term (2 ·)))
  (test-equal (term (set- (1 (2 (3 ·))) (2 (1 ·)))) (term (3 ·)))
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

(define-judgment-form L
  #:mode (lookup I I O)
  #:contract (lookup E s B⊥)
  [-----
   (lookup (s = B⊥ E) s B⊥)]
  [(where #true (≠ s_1 s_2))
   (lookup E s_1 B⊥_1)
   -----
   (lookup (s_2 = B⊥_2 E) s_1 B⊥_1)])

(define-metafunction L
  extend : E s B⊥ -> E
  [(extend · s B⊥) (s = B⊥ ·)]
  [(extend (s_1 = B⊥_1 E) s_1 B⊥_2) (s_1 = B⊥_2 E)]
  [(extend (s_1 = B⊥_1 E) s_2 B⊥_2) (s_1 = B⊥_1 (extend E s_2 B⊥_2))])

(define-metafunction L
  extend-S : E S B⊥ -> E
  [(extend-S E · B⊥) E]
  [(extend-S E (s_1 S) B⊥) (extend-S (extend E s_1 B⊥) S B⊥)])

(define-metafunction L
  remove : E s -> E
  [(remove · s_1) ·]
  [(remove (s_1 = B⊥_1 E) s_1) E]
  [(remove (s_1 = B⊥_1 E) s_2) (s_1 = B⊥_1 (remove E s_2))])

(module+ test
  (test-judgment-holds (lookup (extend (extend (extend · s3 ⊥) s2 ff) s1 tt) s1 tt))
  (test-judgment-holds (lookup (extend (extend (extend · s3 ⊥) s2 ff) s1 tt) s2 ff))
  (test-judgment-holds (lookup (extend (extend (extend · s3 ⊥) s2 ff) s1 tt) s3 ⊥))
  (test-equal (term (extend (extend · s1 ff) s1 tt))
              (term (s1 = tt ·)))
  (test-judgment-holds (lookup (extend-S (extend · s3 ⊥) (s2 (s1 ·)) tt) s2 tt))
  (test-judgment-holds (lookup (extend-S (extend · s3 ⊥) (s2 (s1 ·)) ff) s1 ff))
  (test-judgment-holds (lookup (extend-S (extend · s3 ⊥) (s2 (s1 ·)) ff) s3 ⊥)))

(define-metafunction L
  dom : E -> S
  [(dom ·) ·]
  [(dom (s = B⊥ E)) (∪ (set s) (dom E))])

(module+ test
  (test-equal (term (dom (extend-S (extend · s3 ⊥) (s2 (s1 ·)) tt)))
              (term (s3 (s2 (s1 ·))))))

(define-metafunction L
  parens : any -> any
  [(parens any) any])
