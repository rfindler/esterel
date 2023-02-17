#lang setup/infotab

(define collection 'multi)

(define deps '("esterel-doc" "esterel-examples" "esterel-lib" "esterel-test"))
(define implies '("esterel-doc" "esterel-examples" "esterel-lib" "esterel-test"))

(define pkg-desc "Esterel implementation in Racket")

(define pkg-authors '(robby))

(define license
  '(Apache-2.0 OR MIT))
