#lang racket
(require "structs.rkt")
(module+ test (require rackunit))

(provide
 (contract-out
  [new-search-state (-> search-state?)]

  ;; we need to choose a signal to be absent when `continue!`
  ;; is invoked; make the choice, note the rollback point
  ;; and return the choice
  [continue! (->i ([s search-state?]
                   [rollback any/c]
                   [known-signals (listof signal?)]
                   [choices (non-empty-listof signal?)])
                  [choice signal?])]

  ;; when `fail!` is invoked, we know the last choice that was made didn't work
  ;; out (but we don't know which signal that was chosen was the wrong choice, sadly).
  ;; The result should be a place to roll back to (one of the arguments passed to
  ;; `continue!` in the past) and a new signal to try from that state. Or, if the
  ;; search space has been exhaused, the `choice` result should be #f.
  [fail! (->i ([s search-state?])
              (values [rollback any/c] [choice (or/c #f signal?)]))]
  ))

(define node?/fw (let ([node? (λ (x) (node? x))]) node?))
(define link?/fw (let ([link? (λ (x) (link? x))]) link?))
(define search-tree/c
  (or/c 'unk       ;; -- means that we have not explored here
        'fail      ;; -- means that this was a failure
        node?/fw   ;; -- means that there is more structure here
        ))

(struct/contract
 search-state
 ([root search-tree/c]
  [latest-leaf
   (or/c #f  ;; -- only #f when we've not yet started the search
         link?/fw)])
 #:mutable)

(struct/contract
 node
 ([rollback any/c]
  [known-signals list?]
  [choices list?]
  [children (listof link?/fw)])
 #:transparent)

;; choice : this is the signal we chose to not emit
;; subtree : this is what happened when we explored it
(struct/contract link ([choice signal?] [subtree search-tree/c])
  #:mutable #:transparent)

(define (new-search-state) (search-state 'unk #f))

(define (continue! se-st rollback known-signals choices)
  (cond
    [(equal? (search-state-root se-st) 'unk)
     (define root-node
       (node rollback known-signals choices
             (for/list ([choice (in-list choices)])
               (link choice 'unk))))
     (set-search-state-root! se-st root-node)
     (set-search-state-latest-leaf! se-st (car (node-children root-node)))]
    [else
     (define latest-leaf (search-state-latest-leaf se-st))
     (define new-children
       (for/list ([choice (in-list choices)])
         (link choice 'unk)))
     (define new-node
       (node rollback known-signals choices new-children))
     (set-link-subtree! latest-leaf new-node)
     (set-search-state-latest-leaf! se-st (pick-one new-children))])
  (link-choice (search-state-latest-leaf se-st)))

(define (pick-one l)
  (list-ref l (random (length l))))

(define (fail! se-st)
  (cond
    [(search-state-latest-leaf se-st)
     ;; note that the current leaf failed
     (set-link-subtree! (search-state-latest-leaf se-st) 'fail)

     (let/ec escape
       (define root (search-state-root se-st))

       ;; check if any of the children of the root are unexplored; if so use it
       (for ([child (in-list (node-children (search-state-root se-st)))])
         (when (equal? 'unk (link-subtree child))
           (set-search-state-latest-leaf! se-st child)
           (escape (node-rollback root)
                   (link-choice child))))

       ;; if everything is failed, we know
       ;; this is a non-constructive program
       (when (search-tree-is-all-failed? (search-state-root se-st))
         (escape #f #f))

       ;; since all the children of the root are explored, let's
       ;; just do a depth-first search for the first unexplored
       ;; place and try to explore it.
       (for ([root-child-link (in-list (node-children (search-state-root se-st)))])
         (let loop ([parent root]
                    [link-to-parent root-child-link])
           (define child (link-subtree link-to-parent))
           (match child
             ['unk
              (set-search-state-latest-leaf! se-st link-to-parent)
              (escape (node-rollback parent)
                      (link-choice link-to-parent))]
             ['fail (void)]
             [(node _ _ _ children)
              (for ([link (in-list children)])
                (loop child link))])))
       (pretty-write (search-state-root se-st))
       (error 'fail! "I'm not sure what to do..."))]
    [else
     ;; here we fail even on the first attempt with no choices made
     ;; (this can happen when an exn is raised without looking at
     ;; a signal)
     (values #f #f)]))

(define (search-tree-is-all-failed? st)
  (let loop ([st st])
    (match st
      ['fail #t]
      ['unk #f]
      [(node rollback know-signals choices children)
       (for/and ([child (in-list children)])
         (loop (link-subtree child)))])))

(module+ test
  (define s1 (signal "s1" #f))
  (define s2 (signal "s2" #f))
  (define s3 (signal "s3" #f))
  (define s4 (signal "s3" #f))

  (check-equal?
   (let ([se-st (new-search-state)])
     (continue! se-st 1 '() (list s1)))
   s1)
  (check-equal?
   (let ([se-st (new-search-state)])
     (continue! se-st 1 '() (list s1 s2)))
   s1)
  (check-equal?
   (let ([se-st (new-search-state)])
     (continue! se-st 1 '() (list s1 s2))
     (continue! se-st 1 '() (list s2)))
   s2)

  (check-equal?
   (let ([se-st (new-search-state)])
     (continue! se-st 1 '() (list s1 s2))
     (call-with-values
      (λ () (fail! se-st))
      list))
   (list 1 s2))

  (check-equal?
   (let ([se-st (new-search-state)])
     (continue! se-st 1 '() (list s1 s2))
     (fail! se-st)
     (call-with-values
      (λ () (fail! se-st))
      list))
   (list #f #f))

  ;; this test case overspecifies the search order
  ;; in the sense that it will not pass unless
  ;; we try all of the initial choices (ie rolling
  ;; back to the rollback value `1`) before trying
  ;; anything nested.
  (let ([se-st (new-search-state)])
    (define all (list s1 s2 s3))
    (define c1 (continue! se-st 1 '() all))
    (check-not-false (member c1 all))
    (define c1b (continue! se-st 2 '() (remove c1 all)))
    (check-not-false (member c1b (remove c1 all)))
    (define-values (rb1 c2) (fail! se-st))
    (check-equal? rb1 1)
    (check-not-false (member c2 (remove c1 all)))
    (define-values (rb2 c3) (fail! se-st))
    (check-equal? rb2 1)
    (define-values (rb3 c5) (fail! se-st))
    (check-equal? rb3 2)
    (check-not-false (member c5 (remove c1 all)))
    (define-values (rb4 c6) (fail! se-st))
    (check-equal? rb4 #f)
    (check-equal? c6 #f)
    )

  ;; this test case gets into choices that requires
  ;; jumping around from one place that's not in the
  ;; top-level to another that's also not in the top-level.
  (let ([se-st (new-search-state)])
    (define all (list s1 s2 s3 s4))
    (define c1 (continue! se-st 'top-level '() all))
    (check-not-false (member c1 all))
    (define c1b (continue! se-st 'child1 '() (remove c1 all)))
    (check-not-false (member c1b (remove c1 all)))
    (define-values (rb1 c2) (fail! se-st))
    (check-equal? rb1 'top-level)
    (check-not-false (member c2 (remove c1 all)))
    (define-values (rb2 c3) (fail! se-st))
    (check-equal? rb2 'top-level)
    (define-values (rb3 c4) (fail! se-st))
    (check-equal? rb3 'top-level)
    (check-not-false (member c4 (remove c3 (remove c2 (remove c1 all)))))
    (define-values (rb4 c6) (fail! se-st))
    (check-equal? rb4 'child1)
    (check-not-false (member c6 (remove c1 all)))
    (define c7 (continue! se-st 'child-of-child1 '() (remove c1 (remove c6 all))))
    (check-not-false (member c7 (remove c1 (remove c6 all))))
    (define-values (rb5 c8) (fail! se-st))
    (check-not-false (member rb5 '(child1 child-of-child1)))
    (define-values (rb6 c9) (fail! se-st))
    (check-not-false (member rb6 '(child1 child-of-child1)))
    (check-false (equal? rb5 rb6))
    (define-values (the-end1 the-end2) (fail! se-st))
    (check-false the-end1)
    (check-false the-end2)
    )

  )
