#lang setup/infotab

(define collection 'multi)

(define deps '("base"
               "esterel-lib" "rackunit-lib"
               "parser-tools-lib"
               "html-parsing" "html-lib"))

(define pkg-desc "Tests for the Esterel implementation in Racket")

(define pkg-authors '(robby))

(define version "1.0")

(define license
  '(Apache-2.0 OR MIT))