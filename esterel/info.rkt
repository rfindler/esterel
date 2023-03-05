#lang setup/infotab

(define collection 'multi)

(define deps '("esterel-doc" "esterel-examples" "esterel-lib" "esterel-test" "esterel-rhombus-lib"))
(define implies '("esterel-doc" "esterel-examples" "esterel-lib" "esterel-test" "esterel-rhombus-lib"))

(define pkg-desc "Esterel implementation in Racket")

(define pkg-authors '(robby))

(define license
  '(Apache-2.0 OR MIT))
