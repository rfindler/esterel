#lang racket
(require scriblib/autobib)
(provide (all-defined-out))
(define-cite ~cite citet generate-bibliography)
(define EsterelConstructiveBook
  (make-bib
   #:author "Gérard Berry"
   #:title "The Constructive Semantics of Pure Esterel, Draft Version 3"
   #:date "2002"
   #:url "http://www-sop.inria.fr/members/Gerard.Berry/Papers/EsterelConstructiveBook.pdf"))