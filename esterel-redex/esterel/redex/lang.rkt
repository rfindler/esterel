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
     (! s) (? s) (s ⊃ e)
     (seq e e) (e *) (par e e)
     (trap e)
     nothing
     pause
     (exit N)
     (e \\ s)

     (if e e e)
     B
     N
     (op e e))

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
  (R ::= (Pr S K))

  (op ::= + - < =)
  (k* ::= k B N)
  (K* ::= · (k* K*))
  (R* ::= (Pr S K*)))
