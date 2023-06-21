#lang racket
(require "must-can-hat.rkt" "must-can.rkt"
         "red.rkt" "red-hat.rkt"
         "red-and.rkt"
         "lang.rkt" "helpers.rkt"
         "eval.rkt"
         redex/reduction-semantics)

(define-metafunction L
  p-to-e : p -> e
  [(p-to-e (! s)) (! s)]
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

(define (call-mc fn p)
  (judgment-holds (mc ,fn ,p (⊥E (fv-p ,p)) R) R))
(define (call-mc^ fn e E)
  (judgment-holds (mc^ ,fn ,e ,E R^) R^))
(define (mc-same? fn p)
  (equal? (call-mc fn p)
          (call-mc^ fn
                    (term (p-to-e ,p))
                    (term (⊥E (fv-e (p-to-e ,p)))))))

(define (red p B)
  (judgment-holds (---> ,p E k (S->E ,B (fv-p ,p)) q) (E k (p-to-e q))))
(define (red^ e B)
  (judgment-holds (-->^ ,e E k (S->E ,B (fv-e ,e)) e) (E k e)))

(redex-check
 L (fn p) #:ad-hoc
 (mc-same? (term fn) (term p)))

(redex-check
 L (p B) #:ad-hoc
 (equal? (red (term p) (term B))
         (red^ (term (p-to-e p)) (term B))))

#;
;; this currently fails because an
;; instantaneous infinite loop is generated
;; -- consider if loop safety (6.6, page 69 in constructive semantics book)
;;    would be a good fix for this here
(redex-check
 L
 e
 (begin
   (printf "~s\n" (term e))
   (equal? (judgment-holds (eval e · E) E)
           (judgment-holds (eval& e · E) E))))

(define (can-and-must-preserved-by-red& e print?)
  (define e+Es (judgment-holds (-->&-mc ,e (⊥E (fv-e ,e)) ⊥ e E ⊥) (e E)))
  (for/and ([e+E (in-list e+Es)])
    (match e+E
      [(list e2 E2)
       (define E1 (term (⊥E (fv-e ,e))))
       (when print? (printf "E1:  ~s\n" E1))
       (define Pr1-M (call-mc^ (term Must) e E1))
       (define Pr1-C (call-mc^ (term Can+) e E1))
       (when print? (printf "e2:  ~s\n" e2))
       (when print? (printf "E2:  ~s\n" E2))
       (define Pr2-M (call-mc^ (term Must) e2 E2))
       (define Pr2-C (call-mc^ (term Can+) e2 E2))
       (when print? (printf "Pr-M: ~s ~s\n" Pr1-M Pr2-M))
       (when print? (printf "Pr-C: ~s ~s\n" Pr1-C Pr2-C))
       (and (judgment-holds (Must-grows ,e ,e2))
            (judgment-holds (Can-shrinks ,e ,e2)))])))

(define-judgment-form L
  #:mode (Must-grows I I)
  [(-->&-mc e (⊥E (fv-e e)) ⊥ e_2 E_2 ⊥)
   (mc^ Must e (⊥E (fv-e e)) (Pr S K^))
   (mc^ Must e_2 E_2 (Pr S_2 K^_2))
   (where #t (⊂ (∩ (fv-e e) (∪ (emitted E_2) S_2)) S))
   (where #t (⊂ K^ K^_2))
   ------
   (Must-grows e e_2)])

(define-judgment-form L
  #:mode (Can-shrinks I I)
  [(-->&-mc e (⊥E (fv-e e)) ⊥ e_2 E_2 ⊥)
   (mc^ Can+ e (⊥E (fv-e e)) (Pr S K^))
   (mc^ Can+ e_2 E_2 (Pr S_2 K^_2))
   (where #t (⊂ S (∩ (fv-e e) (∪ (emitted E_2) S_2))))
   (where #t (⊂ K^_2 K^))
   ------
   (Can-shrinks e e_2)])

(define-metafunction L
  emitted : E -> S
  [(emitted ·) ·]
  [(emitted (s = tt E)) (∪ (set s) (emitted E))]
  [(emitted (s = B⊥ E)) (emitted E)])
(module+ test
  (test-equal (term (emitted (extend (extend (extend (extend · s4 tt) s3 ⊥) s2 ff) s1 tt)))
              (term (set s4 s1))))

#|

Must and Can are not preserved by the reduction relation
-->& because of the way that reduction handles signal forms.

For example, consider the term:

   (a \\ a)

Must of this expression contains the result ff, as there are
no emissions of a, so Must recurs with a bound to ff when looking
at the body.

The reduction semantics (and the implementation), however, just
add a = ⊥ to the environment and keep going. In the resulting
term, Must[[a, a = ⊥]] does not produce a value. The reduction
semantics does eventually get the right answer but it takes
more reduction steps before a = ff is added to the environment.

|#


(redex-check
 L
 e
 (can-and-must-preserved-by-red& (term e) #f))
