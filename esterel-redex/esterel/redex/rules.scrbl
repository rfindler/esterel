#lang scribble/base
@(require "typeset.rkt"
          "must-can-star.rkt"
          scriblib/figure
          racket/list
          redex/reduction-semantics)

@title{Must and Can in an Expression-Oriented Language}

@figure["fig:lang" @list{Language}]{
 @(grammar)
}

Beyond the non-terminals mentioned in the grammar, S and K*
are sets of s and k*, respectively. As with the original
semantics, k are the original exit code, but this is
generalized to k* by treating nothing as the unit value and
adding in other values. Thus, an expression can produce
either pause, trap, or produce a value.

S*, K*, and R* play the role of S, K and R in the original,
but generalized in a way corresponding to the generalization
from k to k* and a generalization of S to S* that
accommodates holding a set of values for each signal after
the given expression. For each binding in each of the given
S*'s, the merge-S* function combines all of the elements of
the K* pairwise, using + (much like the Max combination when
computing the exit status of a parallel composition).

E* is like E but in addition to having a B⊥ for
each signal, it also has either ``new'' or ``ready'' each
signal, as well as a value. We skip ``old'', instead
imagining that each signal's value does not carry forward
from instant to instant and is initialized to 0 with + as
the combining operation (so the first emission will
effectively ignore the 0 anyway).

The @tt{?} operator is changed from a conditional to an
operation that returns a boolean, which is decoupled from
the conditional and can be used with @tt{if}.

For the purpose of removing some cases, if a rule is written
with @tt{fn}, then it is a case for Must and both variations
of Can. If the rule is written with Can, then it applies to
both variants of Can. If a rule is written with @tt{fn+},
then it applies to Must and Can+ (this is used only in the
signal rules, where Can⊥ is special). Otherwise, the rule
applies only to the specific function it names.

The @tt{pickfn-seq} returns Can+ when it's first argument is
Can+ and nothing is an element of its second argument.
Otherwise it returns Can⊥.


@(define max-figure-height 600)
@(define max-figure-width 400)
@(define mandatory-breaks '())
@(define-values (signal-rules other-rules)
   (partition (λ (x) (regexp-match? #rx"[\\]" (symbol->string x)))
              (judgment-form->rule-names mc*)))
@(define-values (rest-of-rules1 rules1-pict)
   (format-rules max-figure-width max-figure-height mandatory-breaks
                 #:rules other-rules))
@(define-values (rest-of-rules2 rules2-pict)
   (format-rules max-figure-width max-figure-height mandatory-breaks
                 #:rules rest-of-rules1))
@(define-values (rest-of-signal-rules signal-rules-pict)
   (format-rules max-figure-width max-figure-height mandatory-breaks
                 #:rules signal-rules))
@(when (or rest-of-rules2 rest-of-signal-rules)
   (eprintf "rules.scrbl: more rules remain; need more figures\n"))

@figure["fig:must-can1" @list{Must and Can Rules, i}]{
 @rules1-pict
}

@figure["fig:must-can2" @list{Must and Can Rules, ii}]{
 @rules2-pict
}

@figure["fig:must-can3" @list{Must and Can Rules for Signal Expressions}]{
 @signal-rules-pict
}