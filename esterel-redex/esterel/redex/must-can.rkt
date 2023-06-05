#lang racket
(require redex/reduction-semantics
         "lang.rkt" "helpers.rkt")
(provide mc)

(define-judgment-form L
  #:contract (mc fn p E R)
  #:mode (mc I I I O)
  [---- "k"
   (mc fn k E (Pr (set) (set k)))]

  [---- "!"
   (mc fn (! s) E (Pr (set s) (set nothing)))]

  [(mc fn p E R)
   (lookup E s tt)
   ---- "? tt"
   (mc fn (? s p q) E R)]

  [(mc fn q E R)
   (lookup E s ff)
   ---- "? ff"
   (mc fn (? s p q) E R)]

  [(lookup E s ⊥)
   ---- "Must ? ⊥"
   (mc Must (? s p q) E (Pr (set) (set)))]

  [(mc Can p E (Pr S_p K_p))
   (mc Can q E (Pr S_q K_q))
   (lookup E s ⊥)
   ---- "Can ? ⊥"
   (mc Can (? s p q) E (Pr (∪ S_p S_q) (∪ K_p K_q)))]

  [(mc fn p E R)
   ---- "⊃"
   (mc fn (s ⊃ p) E R)]

  [(mc fn p E (Pr S_p K_p))
   (∉ nothing K_p)
   ---- "; nothing ∉ p"
   (mc fn (seq p q) E (Pr S_p K_p))]

  [(mc Must p E (Pr S_p K_p))
   (∈ nothing K_p)
   (mc Must q E (Pr S_q K_q))
   ---- "Must ; nothing ∈ p"
   (mc Must (seq p q) E
       (Pr (∪ S_p S_q)
           (∪ (set- K_p (set nothing)) K_q)))]

  [(mc Can p E (Pr S_p K_p))
   (∈ nothing K_p)
   (mc Must p E (Pr S_mustp K_mustp))
   (where fn_q (pickfn-seq Can nothing K_mustp))
   (mc fn_q q E (Pr S_q K_q))
   ---- "Can ; nothing ∈ p"
   (mc Can (seq p q) E
       (Pr (∪ S_p S_q)
           (∪ (set- K_p (set nothing)) K_q)))]

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

  [(mc Must p (extend E s ⊥) (Pr S_⊥ K_⊥))
   (∈ s S_⊥)
   (mc Must p (extend E s tt) (Pr S K))
   ---- "Must\\tt"
   (mc Must (p \\ s) E (Pr (set- S (set s)) K))]

  [(mc Can+ p (extend E s ⊥) (Pr S_⊥ K_⊥))
   (∉ s S_⊥)
   (mc Must p (extend E s ff) (Pr S K))
   ---- "Must\\ff"
   (mc Must (p \\ s) E (Pr (set- S (set s)) K))]

  [(mc Must p (extend E s ⊥) (Pr S_m⊥ K_m⊥))
   (mc Can+ p (extend E s ⊥) (Pr S_c⊥ K_c⊥))
   (∉ s S_m⊥)
   (∈ s S_c⊥)
   (mc Must p (extend E s ⊥) (Pr S K))
   ---- "Must\\⊥"
   (mc Must (p \\ s) E (Pr (set- S (set s)) K))]

  [(mc Must p (extend E s ⊥) (Pr S_⊥ K_⊥))
   (∈ s S_⊥)
   (mc Can+ p (extend E s tt) (Pr S K))
   ---- "Can\\tt"
   (mc Can+ (p \\ s) E (Pr (set- S (set s)) K))]

  [(mc Can p (extend E s ⊥) (Pr S_⊥ K_⊥))
   (∉ s S_⊥)
   (mc Can p (extend E s ff) (Pr S K))
   ---- "Can\\ff"
   (mc Can (p \\ s) E (Pr (set- S (set s)) K))]

  [(mc Must p (extend E s ⊥) (Pr S_m⊥ K_m⊥))
   (mc Can+ p (extend E s ⊥) (Pr S_c⊥ K_c⊥))
   (∉ s S_m⊥)
   (∈ s S_c⊥)
   (mc Can+ p (extend E s ⊥) (Pr S K))
   ---- "Can+\\⊥"
   (mc Can+ (p \\ s) E (Pr (set- S (set s)) K))]

  [(mc Can⊥ p (extend E s ⊥) (Pr S K))
   (∈ s S)
   ---- "Can⊥\\⊥"
   (mc Can⊥ (p \\ s) E (Pr (set- S (set s)) K))]
  )


(define-metafunction L
  pickfn-seq : Can any set -> Can
  [(pickfn-seq Can+ any set)
   Can+
   (judgment-holds (∈ any set))]
  [(pickfn-seq Can any set) Can⊥])


(module+ test
  (test-judgment-holds (mc Must nothing · (Pr · (nothing ·))))
  (test-judgment-holds (mc Must (nothing \\ s) · (Pr · (nothing ·))))
  (test-judgment-holds (mc Must ((! s) \\ s) · (Pr · (nothing ·))))
  (test-judgment-holds (mc Must ((? s (! s) (! s)) \\ s) · (Pr · ·)))
  (test-judgment-holds (mc Can+ ((? s (! s) (! s)) \\ s) · (Pr · (nothing ·))))
  (test-judgment-holds (mc Can⊥ ((? s (! s) (! s)) \\ s) · (Pr · (nothing ·))))
  (test-judgment-holds (mc Must (? s (! s) (! s)) (s = ⊥ ·) (Pr · ·)))
  (test-judgment-holds (mc Can+ (? s (! s) (! s)) (s = ⊥ ·) (Pr (s ·) (nothing ·))))
  (test-judgment-holds (mc Can⊥ (? s (! s) (! s)) (s = ⊥ ·) (Pr (s ·) (nothing ·))))
  (test-judgment-holds (mc Must (par (! s) (! t)) (s = ⊥ ·) (Pr (s (t ·)) (nothing ·))))
  (test-judgment-holds (mc Can+ (par (! s) (! t)) (s = ⊥ ·) (Pr (s (t ·)) (nothing ·))))
  (test-judgment-holds (mc Can⊥ (par (! s) (! t)) (s = ⊥ ·) (Pr (s (t ·)) (nothing ·))))

  (test-judgment-holds (mc Must (seq pause (exit 0)) · (Pr · (pause ·))))
  (test-judgment-holds (mc Can+ (seq pause (exit 0)) · (Pr · (pause ·))))
  (test-judgment-holds (mc Can⊥ (seq pause (exit 0)) · (Pr · (pause ·))))
  (test-judgment-holds (mc Must (seq nothing pause)  · (Pr · (pause ·))))
  (test-judgment-holds (mc Can+ (seq nothing pause)  · (Pr · (pause ·))))
  (test-judgment-holds (mc Can⊥ (seq nothing pause)  · (Pr · (pause ·))))

  (test-judgment-holds (mc Can+ (? I nothing pause) (I = tt (O = ⊥ ·)) (Pr · (nothing ·))))
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
  #;
  (show-derivations
   (append
    (build-derivations (mc Can+ ((par (! S) (? S 0 (! O))) \\ S) (O = ⊥ ·) R))
    (build-derivations (mc Can⊥ ((par (! S) (? S 0 (! O))) \\ S) (O = ⊥ ·) R)))
   #:pp pp))
