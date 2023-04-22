#lang racket
(require redex/reduction-semantics)
(provide L)

(define-language L
  (p q ::=
     (! s) (? s p q) (s ⊃ p)
     (seq p q) (p *) (par p q)
     (trap p)
     nothing
     pause
     (exit N)
     (p \\ s))

  (e ::=
     (! s e) s (? s) (s ⊃ e)
     (seq e e) (e *) (par e e)
     nothing pause
     (trap e) (exit N)
     (e \\ s)
     (if e e e)
     B
     N
     (op e e))

  (N ::= natural)
  (k ::= nothing pause (exit N))
  (s ::= variable-not-otherwise-mentioned)

  (k* ::= k B N)
  (B⊥ ::= B ⊥)
  (B ::= tt ff)

  (fn+ ::= Must Can+)
  (fn ::= Must Can)
  (Can ::= Can+ Can⊥)
  (E ::= · (s = B⊥ E))
  (E* ::= · (s = B⊥ status N E*))
  ;; signals are always natural valued with +
  ;; and init set to 0 so we don't need to worry
  ;; about the `old` vs `new` distinction
  (status ::= ready new)
  (set ::= · (any set))
  (S ::= · (s S))
  (K ::= · (k K))
  (R ::= (Pr S K))

  (op ::= + - < =)
  (S* ::= · (s = K* S*))
  (K* ::= · (k* K*))
  (R* ::= (Pr S* K*)))
