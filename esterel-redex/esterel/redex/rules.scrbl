#lang scribble/base
@(require "typeset.rkt" scriblib/figure)

@title{Must and Can in an Expression-Oriented Language}

@figure["fig:lang" @list{Language}]{
 @(grammar)
}

Beyond the non-terminals mentioned in the grammar, S and K*
are sets of s and k*, respectively. As with the original
semantics, k are the original exit code, but this is
generalized to k* by treating nothing as the unit value and
adding in other values. Similarly, K* and R* play the role
of K and R in the original, but generalized in a
corresponding way.

The @tt{?} operator is changed from a conditional to an
operation that returns a boolean, which is decoupled from
the conditional and can be used with @tt{if}.

For the purpose of removing some cases, if a rule is written
with @tt{fn}, then it is a case for Must and both variations
of Can. If the rule is written with Can, then it applies to
both variants of Can. Otherwise, the rule applies only to
the specific function it names.

The @tt{pickfn-seq} returns Can+ when it's first argument is
Can+ and nothing is an element of its second argument.
Otherwise it returns Can⊥.


@(define max-figure-height 600)
@(define max-figure-width 400)
@(define mandatory-breaks '())
@(define-values (rest-of-rules1 rules1)
   (format-rules max-figure-width max-figure-height mandatory-breaks))
@(define-values (rest-of-rules2 rules2)
   (format-rules max-figure-width max-figure-height mandatory-breaks
                 #:rules rest-of-rules1))
@(define-values (rest-of-rules3 rules3)
   (format-rules max-figure-width max-figure-height mandatory-breaks
                 #:rules rest-of-rules2))
@(when rest-of-rules3
   (eprintf "rules.scrbl: more rules remain; need a third figure\n"))

@figure["fig:must-can1" @list{Must and Can Rules, i}]{
 @rules1
}

@figure["fig:must-can2" @list{Must and Can Rules, ii}]{
 @rules2
}

@figure["fig:must-can3" @list{Must and Can Rules, iii}]{
 @rules3
}
