#lang racket
(require "must-can-star.rkt" "must-can.rkt"
         "lang.rkt" "helpers.rkt"
         redex/reduction-semantics)

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

(define (call-mc fn p)
  (judgment-holds (mc ,fn ,p (⊥E (close ,p)) R) R))
(define (call-mc* fn p)
  (judgment-holds (mc* ,fn (p-to-e ,p) (⊥E* (close ,p)) R*) (R*->R R*)))
(define (mc-same? fn p)
  (equal? (call-mc fn p)
          (call-mc* fn p)))
  
(redex-check
 L (fn p) #:ad-hoc
 (mc-same? (term fn) (term p)))
