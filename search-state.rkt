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

;; search-tree : search-tree
;; latest-leaf : #f or search-tree   -- only #f when we've not yet started the search
(struct search-state (root latest-leaf) #:mutable)

;; search-tree = one of:
;;   'unk  -- means that we have not explored here
;;   'fail -- means that this was a failure
;;   node  -- means that there is more structure here

;; children : (listof link?)
(struct node (rollback known-signals choices children) #:transparent)

;; choice : signal -- this is the signal we chose to not emit
;; subtree : search-tree -- this is what happened when we explored it
(struct link (choice [subtree #:mutable]) #:transparent)


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
     (set-search-state-latest-leaf! se-st (car new-children))])
  (link-choice (search-state-latest-leaf se-st)))


(define (fail! se-st)
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

    ;; here we need to do some more searching, but I don't have an algorithm
    ;; for that yet.....
    (pretty-write (search-state-root se-st))
    (error 'fail! "I'm not sure what to do...")))

(define (search-tree-is-all-failed? st)
  (let loop ([st st])
    (match st
      ['fail #t]
      ['unk #f]
      [(node rollback know-signals choices children)
       (for/and ([child (in-list children)])
         (loop (link-subtree child)))])))

(module+ test
  (define s1 (signal "s1"))
  (define s2 (signal "s2"))

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
   (list #f #f)))
