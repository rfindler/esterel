#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/struct
          scribble/eval
          esterel/scribblings/bib)

@title{Esterel in Racket}

@author["Robert Bruce Findler"]

This manual describes an implementation of Esterel that
cooperates with Racket to the best of its ability. In
particular, any side-effect free code should be able to be
mixed with Esterel code and run from within a reaction.

@table-of-contents[]

@include-section["scribblings/eir-for-racket.scrbl"]
@include-section["scribblings/eir-for-esterel.scrbl"]
@include-section["scribblings/kernel.scrbl"]
@include-section["scribblings/full.scrbl"]

@generate-bibliography[]

@index-section[]

@; Needs a timeout for testing:
@(module* test racket/base
   (require (submod ".."))
   (module config info
     (define timeout 300)))
