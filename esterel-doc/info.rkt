#lang setup/infotab

(define collection 'multi)

(define deps '("base" "esterel-lib" "esterel-rhombus-lib"))
(define build-deps '("racket-doc" "scribble-lib" "rhombus" "rhombus-lib" "rhombus-scribble-lib"))
(define pkg-desc "Documentation for the Esterel implementation in Racket")

(define pkg-authors '(robby))

(define version "1.0")

(define license
  '(Apache-2.0 OR MIT))
