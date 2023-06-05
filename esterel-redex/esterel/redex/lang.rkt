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
     (! s) s (s ⊃ e)
     (seq e e) (e *) (par e e)
     nothing pause
     (trap e) (exit N)
     (e \\ s)
     (if e e e)
     B
     N
     (op e e))

  (N ::= natural)
  (k l ::= nothing pause (exit N))
  (k& l& ::= k (blocked S))
  (s ::= variable-not-otherwise-mentioned)

  (k^ ::= k B N)
  (B⊥ ::= B ⊥)
  (B ::= tt ff)

  (fn+ ::= Must Can+)
  (fn ::= Must Can)
  (Can ::= Can+ Can⊥)
  (E F ::= · (s = B⊥ E))
  (set ::= · (any set))
  (S ::= · (s S))
  (K ::= · (k K))
  (R ::= (Pr S K))

  (op ::= + - < =)
  (K^ ::= · (k^ K^))
  (R^ ::= (Pr S K^))

  (E+nc ::= E nonconstructive)
  
  (EC ::=
      hole
      (s ⊃ EC)
      (seq EC e)
      (par EC e)
      (par e EC)
      (trap EC)
      (if EC e e)
      (op EC e)
      (op v EC))
  (v ::= nothing B N (par v v))
  )
