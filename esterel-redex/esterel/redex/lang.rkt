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
  (N ::= natural)
  (k ::= nothing pause (exit N))
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