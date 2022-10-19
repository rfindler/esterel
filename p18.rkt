#lang racket
(require "esterel.rkt")

#|

This is a wild program that has an explanation in Berry's _The Constructive Semantics of Pure Draft Version 3_:

So far, we have seen cases where a statement has two simultaneous incarnations. Nesting lo ops, traps, and parallel statements can lead to more complex situations with multiple reincarnation of parallels and signals. Consider this program, P18. In the first instant, the three pause statements are selected, S1 and S2 are not emitted, and not_S1_and_not_S2 is emitted. In the second instant, the constructive behavior is as follows:

- The three pause statements terminate, S1 and S2 are emitted, and S1_and_S2 is emitted by the present statement.

- The trap T2 is exited by the “exit T2” statement, the inner loop loops, and a new signal S2 is declared. This new signal is absent since it cannot be emitted, while the current S1 is still present. The present statement is instantaneously re-executed with S1 present and S2 absent, and it emits S1_and_not_S2.

- The trap T1 is exited by the “exit T1” statement and the outer loop loops. A new S1 and a new S2 are declared. Since none of these
new signals can b e emitted, the present statement is executed again with S1 and S2 absent, and not_S1_and_not_S1 is emitted.


In this constructive behavior, S1 has two simultaneous incarnations, a present depth incarnation and an absent surface incarnation, while S2 has three incarnations, one present and two absent. In terms of depth and surface, the first one in constructive execution order can be called a “depth-depth” incarnation since it is both in the depth of “signal S1” and in the depth of “signal S2”, the second one can be called a depth-surface incarnation since it is in the depth of “signal S1” and in the surface of “signal S2”, and the third one can b e called a surface-surface incarnation since it is in the surface of both signal declarations. Accordingly, the present statement has three different active incarnations with three different behaviors.

The example can be extended to n signals, in which case the innermost signal has n+1 incarnations. Although such programs rarely occur in practice and can even look pathological, they are constructively correct and they must be correctly translated. [[The reader may question this point and wonder whether reincarnation is of any practical interest. The answer is yes, although we shall not try to prove this statement here. For an example, see [16] where reincarnation is nicely used to handle menus in a menubar.

[16] D. Clément and J. Incerpi. Programming the behavior of graphical objects using Esterel. In TAPSOFT 1989 Springer-Verlag, LNCS 352.

|#

(define s1_and_s2 (signal))
(define s1_and_not_s2 (signal))
(define not_s1_and_s2 (signal))
(define not_s1_and_not_s2 (signal))

(define r
  (reaction
   (let loop ()
     (with-trap t1
       (define s1 (signal))
       (par
        (begin (pause) (emit s1) (exit-trap t1))
        (let loop ()
          (with-trap t2
            (define s2 (signal))
            (par
             (begin (pause) (emit s2) (exit-trap t2))
             (let loop ()
               (if (signal-value s1)
                   (if (signal-value s2)
                       (emit s1_and_s2)
                       (emit s1_and_not_s2))
                   (if (signal-value s2)
                       (emit not_s1_and_s2)
                       (emit not_s1_and_not_s2)))
               (pause)
               (loop))))
          (loop))))
     (loop))))

(define (keep-only-underscores ht)
  (for/hash ([(k v) (in-hash ht)]
             #:when (regexp-match #rx"_" (signal-name k)))
    (values k v)))
(void (react! r))
(for ([i (in-range 200)])
  (unless (equal? (keep-only-underscores (react! r))
                  (hash not_s1_and_not_s2 #t
                        s1_and_s2 #t
                        s1_and_not_s2 #t))
    (error 'p18 "wrong result: ~s"
           (keep-only-underscores (react! r)))))
