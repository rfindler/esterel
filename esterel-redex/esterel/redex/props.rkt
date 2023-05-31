#lang racket
(require "must-can-hat.rkt" "must-can.rkt"
         "red.rkt" "red-hat.rkt"
         "lang.rkt" "helpers.rkt"
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
  (judgment-holds (mc ,fn ,p (⊥E (close ,p)) R) R))
(define (call-mc^ fn p)
  (judgment-holds (mc^ ,fn (p-to-e ,p) (⊥E (close ,p)) R^) R^))
(define (mc-same? fn p)
  (equal? (call-mc fn p)
          (call-mc^ fn p)))

(define (red p B)
  (judgment-holds (---> ,p E k (S->E ,B (close ,p)) q) (E k (p-to-e q))))
(define (red^ p B)
  (judgment-holds (-->^ (p-to-e ,p) E k (S->E ,B (close ,p)) e) (E k e)))

(redex-check
 L (fn p) #:ad-hoc
 (mc-same? (term fn) (term p)))

(redex-check
 L (p B) #:ad-hoc
 (equal? (red (term p) (term B))
         (red^ (term p) (term B))))
