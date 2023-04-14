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

(define-judgment-form L
  #:mode (mc I I I O)
  [---- "k"
   (mc fn k E (Pr · (k ·)))]

  [---- "!"
   (mc fn (! s) E (Pr (s ·) (0 ·)))]

  [(mc fn p E R)
   (where tt (lookup s E))
   ---- "? tt"
   (mc fn (? s p q) E R)]

  [(mc fn q E R)
   (where ff (lookup s E))
   ---- "? ff"
   (mc fn (? s p q) E R)]

  [(where ⊥ (lookup s E))
   ---- "Must ? ⊥"
   (mc Must (? s p q) E (Pr · ·))]

  [(mc Can p E (Pr S_p K_p))
   (mc Can q E (Pr S_q K_q))
   (where ⊥ (lookup s E))
   ---- "Can ? ⊥"
   (mc Can (? s p q) E (Pr (∪ S_p S_q) (∪ K_p K_q)))]

  [(mc fn p E R)
   ---- "⊃"
   (mc fn (s ⊃ p) E R)]

  [(mc fn p E (Pr S_p K_p))
   (where #false (∈ 0 K_p))
   ---- "; 0 ∉ p"
   (mc fn (seq p q) E (Pr S_p K_p))]

  [(mc Must p E (Pr S_p K_p))
   (where #true (∈ 0 K_p))
   (mc Must q E (Pr S_q K_q))
   ---- "Must ; 0 ∈ p"
   (mc Must (seq p q) E
       (Pr (∪ S_p S_q)
           (∪ (set- K_p 0) K_q)))]

  [(mc Can p E (Pr S_p K_p))
   (where #true (∈ 0 K_p))
   (mc Must p E (Pr S_mustp K_mustp))
   (where fn_q (pickfn-seq Can (∈ 0 K_mustp)))
   (mc fn_q q E (Pr S_q K_q))
   ---- "Can ; 0 ∈ p"
   (mc Can (seq p q) E
       (Pr (∪ S_p S_q)
           (∪ (set- K_p 0) K_q)))]

  [(mc fn p E R)
   ---- "*"
   (mc fn (p *) E R)]

  [(mc fn p E (Pr S_p K_p))
   (mc fn q E (Pr S_q K_q))
   ---- "par"
   (mc fn (par p q) E
       (Pr (∪ S_p S_q)
           (Max K_p K_q)))]

  [(mc fn p E (Pr S K))
   ---- "trap"
   (mc fn (trap p) E (Pr S (↓ K)))]

  [(mc Must p (extend s ⊥ E) (Pr S_⊥ K_⊥))
   (where #true (∈ s S_⊥))
   (mc Must p (extend s tt E) (Pr S K))
   ---- "Must\\tt"
   (mc Must (p \\ s) E (Pr (set- S s) K))]

  [(mc Can+ p (extend s ⊥ E) (Pr S_⊥ K_⊥))
   (where #false (∈ s S_⊥))
   (mc Must p (extend s ff E) (Pr S K))
   ---- "Must\\ff"
   (mc Must (p \\ s) E (Pr (set- S s) K))]

  [(mc Must p (extend s ⊥ E) (Pr S_m⊥ K_m⊥))
   (mc Can+ p (extend s ⊥ E) (Pr S_c⊥ K_c⊥))
   (where #false (∈ s S_m⊥))
   (where #true (∈ s S_c⊥))
   (mc Must p (extend s ⊥ E) (Pr S K))
   ---- "Must\\⊥"
   (mc Must (p \\ s) E (Pr (set- S s) K))]

  [(mc Must p (extend s ⊥ E) (Pr S_⊥ K_⊥))
   (where #true (∈ s S_⊥))
   (mc Can+ p (extend s tt E) (Pr S K))
   ---- "Can\\tt"
   (mc Can+ (p \\ s) E (Pr (set- S s) K))]

  [(mc Can p (extend s ⊥ E) (Pr S_⊥ K_⊥))
   (where #false (∈ s S_⊥))
   (mc Can p (extend s ff E) (Pr S K))
   ---- "Can\\ff"
   (mc Can (p \\ s) E (Pr (set- S s) K))]

  [(mc Must p (extend s ⊥ E) (Pr S_m⊥ K_m⊥))
   (mc Can+ p (extend s ⊥ E) (Pr S_c⊥ K_c⊥))
   (where #false (∈ s S_m⊥))
   (where #true (∈ s S_c⊥))
   (mc Can+ p (extend s ⊥ E) (Pr S K))
   ---- "Can+\\⊥"
   (mc Can+ (p \\ s) E (Pr (set- S s) K))]

  [(mc Can⊥ p (extend s ⊥ E) (Pr S K))
   (where #true (∈ s S))
   ---- "Can⊥\\⊥"
   (mc Can⊥ (p \\ s) E (Pr (set- S s) K))]
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
  (test-equal (term (Max (1 (2 (3 ·))) (2 (3 (4 ·)))))
              (term (4 (3 (2 ·)))))
  (test-equal (term (Max (0 (2 (4 (6 ·)))) (1 (3 (5 ·)))))
              (term (5 (3 (1 (2 (4 (6 ·)))))))))

(define-metafunction L
  ↓ : K -> K
  [(↓ ·) ·]
  [(↓ (k K)) (∪ (↓k k) (↓ K))])

(define-metafunction L
  ↓k : k -> K
  [(↓k 0) (0 ·)]
  [(↓k 2) (0 ·)]
  [(↓k 1) (1 ·)]
  [(↓k k) (,(- (term k) 1) ·)])

(module+ test
  (test-equal (term (↓ (11 (12 (15 ·)))))
              (term (10 (11 (14 ·)))))
  (test-equal (term (↓ (0 (2 ·))))
              (term (0 ·)))
  (test-equal (term (↓ (0 (1 ·))))
              (term (0 (1 ·))))
  (test-equal (term (↓ (1 (2 ·))))
              (term (1 (0 ·))))
  (test-equal (term (↓ (0 (1 (2 ·)))))
              (term (0 (1 ·))))
  (test-equal (term (↓ (0 (1 (2 (3 (4 ·)))))))
              (term (0 (1 (2 (3 ·)))))))

(define-metafunction L
  pickfn-seq : Can boolean -> Can
  [(pickfn-seq Can+ #true) Can+]
  [(pickfn-seq Can boolean) Can⊥])

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
  (test-judgment-holds (mc Must 0 · (Pr · (0 ·))))
  (test-judgment-holds (mc Must (0 \\ s) · (Pr · (0 ·))))
  (test-judgment-holds (mc Must ((! s) \\ s) · (Pr · (0 ·))))
  (test-judgment-holds (mc Must ((? s (! s) (! s)) \\ s) · (Pr · ·)))
  (test-judgment-holds (mc Can+ ((? s (! s) (! s)) \\ s) · (Pr · (0 ·))))
  (test-judgment-holds (mc Can⊥ ((? s (! s) (! s)) \\ s) · (Pr · (0 ·))))
  (test-judgment-holds (mc Must (? s (! s) (! s)) (s = ⊥ ·) (Pr · ·)))
  (test-judgment-holds (mc Can+ (? s (! s) (! s)) (s = ⊥ ·) (Pr (s ·) (0 ·))))
  (test-judgment-holds (mc Can⊥ (? s (! s) (! s)) (s = ⊥ ·) (Pr (s ·) (0 ·))))
  (test-judgment-holds (mc Must (par (! s) (! t)) (s = ⊥ ·) (Pr (s (t ·)) (0 ·))))
  (test-judgment-holds (mc Can+ (par (! s) (! t)) (s = ⊥ ·) (Pr (s (t ·)) (0 ·))))
  (test-judgment-holds (mc Can⊥ (par (! s) (! t)) (s = ⊥ ·) (Pr (s (t ·)) (0 ·))))

  (test-judgment-holds (mc Must (seq 1 2) · (Pr · (1 ·))))
  (test-judgment-holds (mc Can+ (seq 1 2) · (Pr · (1 ·))))
  (test-judgment-holds (mc Can⊥ (seq 1 2) · (Pr · (1 ·))))
  (test-judgment-holds (mc Must (seq 0 1) · (Pr · (1 ·))))
  (test-judgment-holds (mc Can+ (seq 0 1) · (Pr · (1 ·))))
  (test-judgment-holds (mc Can⊥ (seq 0 1) · (Pr · (1 ·))))

  (test-judgment-holds (mc Can+ (? I 0 1) (I = tt (O = ⊥ ·)) (Pr · (0 ·))))
  )

(module+ main
  (require redex/gui)

  (define P8b
    (term
     (seq (trap
           (par
            (seq (? I 0 1)
                 (! O))
            (? O 0 2)))
          (! O))))

  (define (pp exp output-port width txt)
    (define str
      (match exp
        [`(mc ,fn ,p ,E (Pr ,S ,K))
         (~a fn "[" (pw p #:width width #:indent 6) "," "\n"
             "     " (E->str E) "]\n"
             "= ⟨" (set->string S) "," (set->string K) "⟩")]))
    (display str output-port))

  (define (pw p #:width width #:indent indent)
    (define op (open-output-string))
    (parameterize ([pretty-print-columns width]
                   [pretty-print-size-hook
                    (λ (val _1 _2)
                      (and (equal? val '\\) 1))]
                   [pretty-print-print-hook
                    (λ (val mode out-port)
                      (display "\\" out-port))]
                   [pretty-print-print-line
                    (λ (line-number op len dest)
                      (cond
                        [(or (equal? line-number 0)
                             (not line-number))
                         0]
                        [else
                         (newline op)
                         (for ([i (in-range indent)])
                           (display " " op))
                         indent]))])
      (pretty-write p op))
    (get-output-string op))

  (define (E->str e)
    (~a
     "{"
     (let loop ([e e])
       (match e
         ['· ""]
         [`(,p = ,v ·) (~a (~s p) "=" (~s v))]
         [`(,p = ,v ,more) (~a (~s p) "=" (~s v) "," (loop more))]))
     "}"))

  (define (set->string s)
    (define eles
      (let loop ([s s])
        (match s
          ['· '()]
          [`(,s ,S) (cons s (loop S))])))
    (~a "{"
        (apply
         string-append
         (add-between (map ~s (sort eles string<? #:key ~s))
                      ","))
        "}"))

  #;  #;
  (derivation/ps
   (car (build-derivations (mc Must (,P8b \\ O) (I = tt ·) R)))
   "must.ps"
   #:pp pp)
  (derivation/ps
   (car (build-derivations (mc Can+ (,P8b \\ O) (I = tt ·) R)))
   "can.ps"
   #:pp pp)

  #; #;
  (show-derivations
   (build-derivations (mc Can+ (,P8b \\ O) (I = tt ·) R))
   #:pp pp)
  (show-derivations
   (build-derivations (mc Must (,P8b \\ O) (I = tt ·) R))
   #:pp pp)

  ;; this is an example where Can+ and Can⊥ differ
  (show-derivations
   (append
    (build-derivations (mc Can+ ((par (! S) (? S 0 (! O))) \\ S) (O = ⊥ ·) R))
    (build-derivations (mc Can⊥ ((par (! S) (? S 0 (! O))) \\ S) (O = ⊥ ·) R)))
   #:pp pp))
