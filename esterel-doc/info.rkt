#lang setup/infotab

(define collection 'multi)

(define deps '("base" "esterel-lib"))
(define build-deps '("scribble-lib" "racket-doc"))

(define pkg-desc "Documentation for the Esterel implementation in Racket")

(define pkg-authors '(robby))

(define version "1.0")

(define license
  '(Apache-2.0 OR MIT))
