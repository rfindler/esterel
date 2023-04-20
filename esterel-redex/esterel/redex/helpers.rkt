#lang racket
(require redex/reduction-semantics "lang.rkt")
(provide Max ↓
         lookup extend
         lookup* lookup*-B⊥ lookup*-value extend*
         ∈ ∉ ∪ set- set
         ⊥E ⊥E* close
         dom remove-from-dom
         op-each-pair
         merge-S* update-S*
         parens)

(define-metafunction L
  close : p -> S
  [(close (! s)) (s ·)]
  [(close (? s p q)) (∪ (s ·) (∪ (close p) (close q)))]
  [(close (s ⊃ p)) (∪ (s ·) (close p))]
  [(close (seq p q)) (∪ (close p) (close q))]
  [(close (p *)) (close p)]
  [(close (par p q)) (∪ (close p) (close q))]
  [(close (trap p)) (close p)]
  [(close nothing) ·]
  [(close pause) ·]
  [(close (exit N)) ·]
  [(close (p \\ s)) (set- (close p) s)])

(module+ test
  (test-equal (term (close (? s (! t) (trap (w ⊃ ((! z) *))))))
              (term (s (t (w (z ·))))))
  (test-equal (term (close ((! t) \\ t)))
              (term ·)))

(define-metafunction L
  ⊥E : S -> E
  [(⊥E ·) ·]
  [(⊥E (s S)) (s = ⊥ (⊥E S))])

(define-metafunction L
  ⊥E* : S -> E*
  [(⊥E* ·) ·]
  [(⊥E* (s S)) (s = ⊥ new 0 (⊥E* S))])

(define-metafunction L
  dom : S* -> S
  [(dom ·) ·]
  [(dom (s = K* S*)) (s (dom S*))])

(define-metafunction L
  merge-S* : S* S* -> S*
  [(merge-S* · S*) S*]
  [(merge-S* (s = K* S*_1) S*_2)
   (update-S* (merge-S* S*_1 S*_2) s K*)])

(define-metafunction L
  update-S* : S* s K* -> S*
  [(update-S* S* s K*)
   (s = K* S*)
   (judgment-holds (∉ s (dom S*)))]
  [(update-S* (s = K*_1 S*) s K*_2)
   (s = (op-each-pair + K*_1 K*_2) S*)]
  [(update-S* (s_1 = K*_1 S*) s_2 K*_2)
   (s_1 = K*_1 (update-S* S* s_2 K*_2))])

(module+ test
  (test-equal (term (update-S* · s (0 ·))) (term (s = (0 ·) ·)))
  (test-equal (term (update-S* (s = (1 ·) ·) s (2 ·))) (term (s = (3 ·) ·)))
  (test-equal (term (update-S* (s = (1 (2 ·)) ·) s (3 ·))) (term (s = (4 (5 ·)) ·)))
  (test-equal (term (update-S* (s = (3 ·) ·) s (1 (2 ·)))) (term (s = (4 (5 ·)) ·)))
  (test-equal (term (update-S* (s = (1 ·) (t = (1 ·) (u = (1 ·) ·))) t (2 ·)))
              (term (s = (1 ·) (t = (3 ·) (u = (1 ·) ·)))))
  (test-equal (term (update-S* (s = (1 ·) (t = (1 ·) ·)) u (2 ·)))
              (term (u = (2 ·) (s = (1 ·) (t = (1 ·) ·)))))
  (test-equal (term (merge-S* (a = (1 ·) (b = (2 ·) ·))
                              (c = (3 ·) (d = (4 ·) ·))))
              (term (a = (1 ·) (b = (2 ·) (c = (3 ·) (d = (4 ·) ·)))))))

(define-metafunction L
  remove-from-dom : S* s -> S*
  [(remove-from-dom · s) ·]
  [(remove-from-dom (s = K* S*) s) S*]
  [(remove-from-dom (s_1 = K*_1 S*) s_2)
   (s_1 = K*_1 (remove-from-dom S* s_2))])
(module+ test
  (test-equal (term (remove-from-dom (s1 = (3 ·) (s2 = (4 ·) (s3 = (5 ·) ·))) s2))
              (term (s1 = (3 ·) (s3 = (5 ·) ·))))
  (test-equal (term (remove-from-dom (s1 = (3 ·) (s2 = (4 ·) (s3 = (5 ·) ·))) s4))
              (term (s1 = (3 ·) (s2 = (4 ·) (s3 = (5 ·) ·))))))

(define-metafunction L
  op-each-pair : op K* K* -> K*
  [(op-each-pair op · K*) ·]
  [(op-each-pair op (k* K*_1) K*_2)
   (∪ (op-each op k* K*_2) (op-each-pair op K*_1 K*_2))])

(define-metafunction L
  op-each : op k* K* -> K*
  [(op-each op k* ·) ·]
  [(op-each op k*_1 (k*_2 K*))
   (∪ (set (δ op k*_1 k*_2)) (op-each op k*_1 K*))])

(define-metafunction L
  δ : op k* k* -> k*
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
  Max : K* K* -> K*
  [(Max · K) ·]
  [(Max (k K_1) K_2)
   (∪ (Max-kK k K_2)
      (Max K_1 K_2))])

(define-metafunction L
  Max-kK : k* K* -> K*
  [(Max-kK k* ·) ·]
  [(Max-kK k*_1 (k*_2 K*)) (∪ (Max-kK k*_1 K*) ((Max-kk k*_1 k*_2) ·))])

(define-metafunction L
  Max-kk : k* k* -> k*
  [(Max-kk (exit N_1) (exit N_2)) (exit ,(max (term N_1) (term N_2)))]
  [(Max-kk (exit N) k*) (exit N)]
  [(Max-kk k* (exit N)) (exit N)]
  [(Max-kk k* pause) pause]
  [(Max-kk pause k*) pause]

  ;; just declare that `par` discards the results of
  ;; the branches and always returns `nothing`
  [(Max-kk nothing k*) nothing]
  [(Max-kk k* nothing) nothing])

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
  [(↓ (k K)) (∪ (↓k k) (↓ K))])

(define-metafunction L
  ↓k : k -> K
  [(↓k nothing) (nothing ·)]
  [(↓k (exit 0)) (nothing ·)]
  [(↓k pause) (pause ·)]
  [(↓k (exit N)) ((exit ,(- (term N) 1)) ·)])

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
  [(∪ (any set_1) set_2) (any (∪ set_1 (set- set_2 any)))])
(module+ test
  (test-equal (term (∪ (a (b ·)) (c (d ·))))
              (term (a (b (c (d ·)))))))

(define-metafunction L
  set- : set any -> set
  [(set- · any) ·]
  [(set- (any set) any) set]
  [(set- (any_1 set) any_2) (any_1 (set- set any_2))])

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

(module+ test
  (test-judgment-holds (lookup (extend (extend (extend · s3 ⊥) s2 ff) s1 tt) s1 tt))
  (test-judgment-holds (lookup (extend (extend (extend · s3 ⊥) s2 ff) s1 tt) s2 ff))
  (test-judgment-holds (lookup (extend (extend (extend · s3 ⊥) s2 ff) s1 tt) s3 ⊥))
  (test-equal (term (extend (extend · s1 ff) s1 tt))
              (term (s1 = tt ·))))

(define-judgment-form L
  #:mode (lookup*-B⊥ I I O)
  #:contract (lookup*-B⊥ E* s B⊥)
  [(lookup* E* s (B⊥ status N))
   ------
   (lookup*-B⊥ E* s B⊥)])

(define-judgment-form L
  #:mode (lookup*-value I I O O)
  #:contract (lookup*-value E* s status N)
  [(lookup* E* s (B⊥ status N))
   ------
   (lookup*-value E* s status N)])

(define-judgment-form L
  #:mode (lookup* I I O)
  #:contract (lookup* E* s (B⊥ status N))
  [-------------
   (lookup* (s = B⊥ status N E*) s (B⊥ status N))]

  [(where #true (≠ s_1 s_2))
   (lookup* E* s_1 (B⊥_1 status_1 N_1))
   ------
   (lookup* (s_2 = B⊥_2 status N E*) s_1 (B⊥_1 status_1 N_1))])

(define-metafunction L
  extend* : E* s B⊥ -> E*
  [(extend* · s B⊥) (s = B⊥ new 0 ·)]
  [(extend* (s_1 = B⊥_1 status_1 N_1 E*) s_1 B⊥_2) (s_1 = B⊥_2 status_1 N_1 E*)]
  [(extend* (s_1 = B⊥_1 status_1 N_1 E*) s_2 B⊥_2) (s_1 = B⊥_1 status_1 N_1 (extend* E* s_2 B⊥_2))])

(module+ test
  (test-judgment-holds
   (lookup* (extend* (extend* (extend* · s3 ⊥) s2 ff) s1 tt) s1 (tt new 0)))
  (test-judgment-holds
   (lookup* (extend* (extend* (extend* · s3 ⊥) s2 ff) s1 tt) s2 (ff new 0)))
  (test-judgment-holds
   (lookup* (extend* (extend* (extend* · s3 ⊥) s2 ff) s1 tt) s3 (⊥ new 0)))
  (test-equal (term (extend* (extend* · s1 ff) s1 tt))
              (term (s1 = tt new 0 ·))))

(define-metafunction L
  parens : any -> any
  [(parens any) any])