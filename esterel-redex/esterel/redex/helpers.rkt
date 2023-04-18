#lang racket
(require redex/reduction-semantics "lang.rkt")
(provide Max ↓
         lookup extend
         ∈ ∉ ∪ set- set
         ⊥E close)

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
  Max : K* K* -> K*
  [(Max · K) ·]
  [(Max K ·) ·]
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
  (test-equal (term (Max (pause ((exit 0) ((exit 1) ·))) ((exit 0) ((exit 1) ((exit 2) ·)))))
              (term ((exit 2) ((exit 1) ((exit 0) ·)))))
  (test-equal (term (Max (nothing ((exit 0) ((exit 2) ((exit 4) ·)))) (pause ((exit 1) ((exit 3) ·)))))
              (term ((exit 3) ((exit 1) (pause ((exit 0) ((exit 2) ((exit 4) ·)))))))))

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