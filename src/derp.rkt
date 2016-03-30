#lang racket

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require racket/unsafe/ops)
(require racket/shared)
(require racket/performance-hint)
(require racket/pretty)
(require "util/unsafe-struct.rkt")
(require "util/define-case.rkt")
(require "util/fast-for.rkt")

;;;;;;;;;;;;;;;; Nodes ;;;;;;;;;;;;;;;;

;; A grammar is represented by a graph of nodes.
;; Note that nodes have a uniform representation
;; so when optimizing a node, we can just overwrite
;; it and be sure that any nodes pointing to this
;; node will see the changed version.

(unsafe-struct* node
  (tag ;; A tag representing what kind of node it is.  See "case-node-tag".
   child1 child2 ;; Fields for child nodes or other data that the node type might need
   nullable ;; A flag indicating whether this node represents a language containing the empty string.
            ;; See "nullable-true", "nullable-unvisited" and "nullable-false".
   [listeners #:auto] ;; A list of nodes that should be notified if this node is nullable
   [key #:auto] [value #:auto]) ;; A forgetful, one-entry hash table that is used by various algorithms during parsing
  #:constructor-name make-node
  #:mutable #:transparent ;; Mutable so we can set! fields.  Transparent so we can debug with a printed representation.
  #:auto-value '()) ;; Initial value for "listeners", "key", and "value"

;; Set the tag, child1, and child2 of a node.  Note that we do not set node-nullable.
(define-inline (node-partial-set! node tag child1 child2)
  (node-tag-set! node tag)
  (node-child1-set! node child1)
  (node-child2-set! node child2))

;; Copy the tag, child1, child2, and *nullable* from src to dst
(define-inline (node-partial-copy! dst src)
  (node-tag-set! dst (node-tag src))
  (node-child1-set! dst (node-child1 src))
  (node-child2-set! dst (node-child2 src))
  (node-nullable-set! dst (node-nullable src)))

;; Add a listener to or remove all listeners from a node
(define-inline (node-add-listener n listener) (node-listeners-set! n (cons listener (node-listeners n))))
(define-inline (node-clear-listeners n)       (node-listeners-set! n '()))

;; Symbolic definitions of the node tags
(define-case case-node-tag ;; A case construct just for node-tags
  empty-tag ;; Tag for the empty language containing no strings
  eps-tag ;; Tag for the epsilon language containing only the empty string
  token-tag ;; Tag for the language containing single token strings matching a particular predicate
  alt-tag ;; Tag for the "or" language containing the union of two other languages
  seq-tag ;; Tag for the "concatenation" language containing strings concatenated from the elements of two other languages
  red-tag ;; Tag for the "reduction" language containing the same strings as another language
          ;; but with a function applied to the resulting parse trees
  unknown-tag) ;; Tag for a node whose type we do not yet know.  Used only in "derive" to prevent other
               ;; nodes from copying the children of the current node (such as happens when optimizing)
               ;; when we do not yet know what those children are.

;; Possible values for node-nullable
(define nullable-true #t) ;; Node is definitely nullable
(define nullable-false (list 'nullable-false-gensym)) ;; Node is definitely not nullable
;; NOTE: All cons pairs are treated as nullable-false unless we are currently in "nullable?" and it is the "visited" pair.
(define nullable-unvisited #f) ;; Nullability is not yet computed for this node and is unknown

;;;;;;;; Dumb constructors and mutators ;;;;;;;;

;; NOTE: These have to be macros so the "shared" construct used by "grammar" and "language" works okay
(define-syntax make-empty-node   (syntax-rules () [(_)            an-empty-node])) ;; These nodes are all the same as each other
(define an-empty-node                                             (make-node empty-tag   #f #f      nullable-false    ))
(define-syntax make-eps-node     (syntax-rules () [(_ content)    (make-node eps-tag     content #f nullable-true     )]))
(define-syntax make-token-node   (syntax-rules () [(_ pred class) (make-node token-tag   pred class nullable-false    )]))
(define-syntax make-alt-node     (syntax-rules () [(_ left right) (make-node alt-tag     left right nullable-unvisited)]))
(define-syntax make-seq-node     (syntax-rules () [(_ left right) (make-node seq-tag     left right nullable-unvisited)]))
(define-syntax make-red-node     (syntax-rules () [(_ child func) (make-node red-tag     child func nullable-unvisited)]))
(define-syntax make-unknown-node (syntax-rules () [(_)            (make-node unknown-tag #f #f      nullable-unvisited)]))

;; Mutate a node to become a certain type.  We do not set the listeners, key or value as they should not currently be live
(define-inline (empty-node-set! node)            (node-partial-set! node empty-tag #f #f)      (node-nullable-set! node nullable-false))
(define-inline (eps-node-set!   node content)    (node-partial-set! node eps-tag   content #f) (node-nullable-set! node nullable-true))
(define-inline (token-node-set! node pred class) (node-partial-set! node token-tag pred class) (node-nullable-set! node nullable-false))
(define-inline (alt-node-set!   node left right) (node-partial-set! node alt-tag   left right))
(define-inline (seq-node-set!   node left right) (node-partial-set! node seq-tag   left right))
(define-inline (red-node-set!   node child func) (node-partial-set! node red-tag   child func))

;;;;;;;; Smart/optimizing constructors and mutators ;;;;;;;;

;; Construct a seq-node but check if we can reduce it first
(define-inline (make/opt-seq-node left right)
  (case-node-tag (node-tag left)
    [empty-tag (make-empty-node)]
    [seq-tag (make-red-node (make-seq-node (node-child1 left) (make-seq-node (node-child2 left) right))
                            (lambda (ts) (for/list ([t ts]) (cons (cons (car t) (cadr t)) (cddr t)))))]
    [eps-tag (make-red-node right (let ([e (node-child1 left)])
                                    (lambda (ts2) (for+/list ([t1 (force e)][t2 ts2]) (cons t1 t2)))))]
    [red-tag (make-red-node (make-seq-node (node-child1 left) right)
                            (let ([f (node-child2 left)])
                              (lambda (ts) (for*/list ([t ts][t+ (f (list (car t)))]) (cons t+ (cdr t))))))]
    [else (make-seq-node left right)]))

;; Construct a red-node but check if we can reduce it first
(define-inline (make/opt-red-node child func)
  (case-node-tag (node-tag child)
    [empty-tag (make-empty-node)]
    [eps-tag (make-eps-node (let ([e (node-child1 child)]) (delay (func (force e)))))]
    [red-tag (make-red-node (node-child1 child) (compose1 func (node-child2 child)))]
    [else (make-red-node child func)]))

;; Mutate a node to become a red-node but check if we can reduce it first. Returns true iff any reduction was done.
(define-inline (red-node-set/opt! node child func)
  (case-node-tag (node-tag child)
    [empty-tag (empty-node-set! node) #t]
    [eps-tag (eps-node-set! node (let ([e (node-child1 child)]) (delay (func (force e))))) #t]
    [red-tag (red-node-set! node (node-child1 child) (compose1 func (node-child2 child))) #t]
    [else (red-node-set! node child func) #f]))

;; Mutate a node to become an alt-node but check if we can reduce it first. Returns true iff any reduction was done.
(define-inline (alt-node-set/opt! node left right)
  (case-node-tag (node-tag left)
    [eps-tag (case-node-tag (node-tag right)
               [eps-tag (eps-node-set! node (let ([e1 (node-child1 left)]
                                                  [e2 (node-child1 right)])
                                              (delay (append (force e1) (force e2)))))
                        #t]
               [else (alt-node-set! node left right) #f])]
    [empty-tag (node-partial-copy! node right) #t]
    [else (case-node-tag (node-tag right)
            [empty-tag (node-partial-copy! node left) #t]
            [else (alt-node-set! node left right) #f])]))

;; Mutate a node to become a seq-node but check if we can reduce it first. Returns true iff any reduction was done.
;;
;; NOTE: We check only the left child and not the right for reductions as
;; during parsing only the left child could change to something that we could
;; reduce with. The initial optimization has extra code to check the right child.
(define-inline (seq-node-set/opt-left! node left right)
  (case-node-tag (node-tag left)
    [empty-tag (empty-node-set! node) #t]
    [seq-tag (red-node-set! node (make/opt-seq-node (node-child1 left) (make/opt-seq-node (node-child2 left) right))
                            (lambda (ts) (for/list ([t ts]) (cons (cons (car t) (cadr t)) (cddr t)))))
             #t]
    [eps-tag (red-node-set! node right
                            (let ([e (node-child1 left)])
                              (lambda (ts2) (for+/list ([t1 (force e)][t2 ts2]) (cons t1 t2)))))
             #t]
    [red-tag (red-node-set! node (make/opt-seq-node (node-child1 left) right)
                            (let ([f (node-child2 left)])
                              (lambda (ts) (for*/list ([t ts][t+ (f (list (car t)))]) (cons t+ (cdr t))))))
             #t]
    [else (seq-node-set! node left right) #f]))

;;;;;;;;;;;;;;;; Optimize ;;;;;;;;;;;;;;;;

;; Optimize an entire grammar.  Should be used on the grammar before parsing.
(define optimize-gensym (list 'optimize-gensym)) ;; Gensym so we know if we've visited a node before
(define (optimize l)
  (when (not (eq? (node-key l) optimize-gensym)) ;; Skip over nodes we've already visited
    (node-key-set! l optimize-gensym) ;; Mark the node as visited
    (define reoptimize
      (case-node-tag (node-tag l)
        [red-tag
         (optimize (node-child1 l))
         (red-node-set/opt! l (node-child1 l) (node-child2 l))]
        [alt-tag
         (optimize (node-child1 l))
         (optimize (node-child2 l))
         (alt-node-set/opt! l (node-child1 l) (node-child2 l))]
        [seq-tag
         (optimize (node-child1 l))
         (optimize (node-child2 l))
         (or (seq-node-set/opt-left! l (node-child1 l) (node-child2 l))
             ;; Extra reductions for the right hand child that are not done while parsing
             (case-node-tag (node-tag (node-child2 l))
               [empty-tag (empty-node-set! l) #t]
               [eps-tag (red-node-set! l (node-child1 l)
                                       (let ([e (node-child1 (node-child2 l))])
                                         (lambda (ts1) (for+/list ([t1 ts1][t2 (force e)]) (cons t1 t2)))))
                        #t]
               [red-tag (red-node-set! l (make-seq-node (node-child1 l) (node-child1 (node-child2 l)))
                                       (let ([f (node-child2 (node-child2 l))])
                                         (lambda (ts) (for*/list ([t ts][t+ (f (list (cdr t)))]) (cons (car t) t+)))))
                        #t]
               [else #f]))]
        [else #f]))
    (if reoptimize
        (begin ;; If a reduction occurred, re-run the optimization
          (node-key-set! l '()) ;; But first mark the node as unvisited so we don't skip it
          (optimize l))
        (begin ;; If no reduction occurred, compute the nullability of the node and then return
          (nullable? l)
          (void)))))

;;;;;;;;;;;;;;;; Parse ;;;;;;;;;;;;;;;;

;; The main parsing function.  It is just a trivial loop over "derive".
(define parse
  (case-lambda
   [(l s) (parse l s values)]
   [(l s f)
    (let loop ([l l])
      (define c (s))
      (f l c)
      (if (eof-object? c)
          (parse-tree l)
          (loop (derive l c))))]))

;;;;;;;;;;;;;;;; Derive ;;;;;;;;;;;;;;;;

;; Compute the derivative of the language l by the token c
(define (derive l c)

  ;; A common pattern in this code is to:
  ;;  1. allocate a node,
  ;;  2. set its key and value,
  ;;  3. recur into any children,
  ;;  4. set the allocated node to the result using those children (after possible optimization),
  ;;  5. and return the allocated node.
  ;; This macro abstracts out this pattern, and does steps 2 and 5
  ;; automatically while taking step 1 as a parameter.
  (define-syntax let-result
    (syntax-rules ()
      [(_ ([lhs rhs]) stmt ...)
       (let ([lhs rhs])
         (node-key-set! l c)
         (node-value-set! l lhs)
         stmt ...
         lhs)]))

  (if (eq? c (node-key l)) ;; Check to see if we already know the derivative.  If so, return it.
      (node-value l)
      (case-node-tag (node-tag l)
       ;; Derivatives of empty, eps, and token are straightforward and are directly computed
       [empty-tag (let-result ([result (make-empty-node)]))]
       [eps-tag (let-result ([result (make-empty-node)]))]
       [token-tag (let-result ([result (if ((node-child1 l) c)
                                           (make-eps-node (delay (list c)))
                                           (make-empty-node))]))]
       ;; Derivatives of red and alt recur into their children and then mutate "result" with the answer.
       ;;
       ;; NOTE: We allocate "result" before the recursions so cycles can point
       ;; to this node even though we are still calculating it.  We use an "unknown"
       ;; node so that those cycles don't try to optimize and copy the children of "result"
       ;; as we don't know what they are yet.
       [red-tag (let-result ([result (make-unknown-node)])
                  (red-node-set/opt! result (derive (node-child1 l) c) (node-child2 l)))]
       [alt-tag (let-result ([result (make-unknown-node)])
                  (alt-node-set/opt! result (derive (node-child1 l) c) (derive (node-child2 l) c)))]
       ;; Derivatives of seq are like for red and alt, but we have two
       ;; cases depending on whether the first child is nullable
       [seq-tag
        (let-result ([result (make-unknown-node)])
          (if (nullable? (node-child1 l))
              (alt-node-set/opt! result (make/opt-red-node (derive (node-child2 l) c)
                                                           (let ([child (node-child1 l)])
                                                             (lambda (ts2)
                                                               (for+/list ([t1 (parse-tree child)]
                                                                           [t2 ts2])
                                                                          (cons t1 t2)))))
                                 (make/opt-seq-node (derive (node-child1 l) c) (node-child2 l)))
              (seq-node-set/opt-left! result (derive (node-child1 l) c) (node-child2 l))))]
       [else (error 'derive "unknown node type: ~s" (node-tag l))])))

;;;;;;;;;;;;;;;; Nullable ;;;;;;;;;;;;;;;;

;; Returns whether a node is nullable.  Uses the node-nullable cache if possible.
;;
;; NOTE: We use a trick here where old gensyms represent definitely non-nullable values.
;; This prevents us from having to go back and manually set them.
(define-inline (nullable? node) (cached-nullable (list 'nullable?-gensym) #f node))

;; Checks the cache before calculating nullability.  The "visited" gensym is used see if we have
;; visited this node before.  The "parent" variable stores the last node that we were at
;; so we can add it as a listener if need be.
(define-inline (cached-nullable visited parent node)
  (define nullable (node-nullable node))
  (cond
   [(eq? nullable nullable-true) #t] ;; Cached value is already known and is true
   [(pair? nullable) ;; Cached value is either nullable-false or visited
    ;; Use #f as our answer, but if the cached value is visited and we have a parent
    ;; then we might need to revise our answer later, so add "parent" as a listener.
    (when (and parent (eq? nullable visited)) (node-add-listener parent node))
    #f]
   [else ;; This is our first time visiting the node
    (node-nullable-set! node visited) ;; Mark it as visited
    ;; Try directly computing the nullability
    (if (compute/notify-nullable visited node)
        #t
        ;; If we got #f as the answer, it might later be updated to #t
        ;; so add our parent as listener if we have one.
        (begin (when parent (node-add-listener parent node)) #f))]))

;; Compute nullability and notify listeners if needed
(define (compute/notify-nullable visited node)
  ;; Do a direct computation of nullability from compute-nullable
  (if (compute-nullable visited node)
      ;; If it comes back true, set the cache and recompute the nullability of any listeners
      (begin (node-nullable-set! node nullable-true)
             (for ([l (node-listeners node)]) (compute/notify-nullable visited l))
             (node-clear-listeners node) ;; Listeners are no longer needed so clear them
             #t)
      #f))

;; Kernel of the nullability computation.  Has the rules for specific node types.
(define-inline (compute-nullable visited node)
  (case-node-tag (node-tag node)
   [eps-tag #t]
   [empty-tag #f]
   [token-tag #f]
   [seq-tag (and (cached-nullable visited node (node-child1 node)) (cached-nullable visited node (node-child2 node)))]
   [alt-tag (or (cached-nullable visited node (node-child1 node)) (cached-nullable visited node (node-child2 node)))]
   [red-tag (cached-nullable visited node (node-child1 node))]
   [else (error 'nullable? "Unexpected node type: ~s" node)]))

;;;;;;;;;;;;;;;; Parse Tree ;;;;;;;;;;;;;;;;

;; Constructs the set of parse trees for a node.  Used only at the end of parsing.

(define parse-tree-gensym (list 'parse-tree-gensym))
(define (parse-tree l)
  (cond
   [(eq? parse-tree-gensym (node-key l)) (node-value l)] ;; If we've visited this node before, return the cached result
   [(not (nullable? l)) '()] ;; If the node is not nullable, then it has no parse trees.
   [else
    (node-key-set! l parse-tree-gensym) ;; Mark the node as visited before recurring
    (node-value-set! l '()) ;; Anything that comes back here is a cycle and should be eliminated
                            ;; so we temporarily set our list of parse trees to be empty
    (define result
      (case-node-tag (node-tag l)
        ;; Empty and token nodes are not completed parses so return an empty list for those
        [empty-tag '()]
        [token-tag '()]
        ;; Epsilon is a completed parse so return its stored value
        [eps-tag (force (node-child1 l))]
        ;; Reductions apply their function to the result of their child
        [red-tag ((node-child2 l) (parse-tree (node-child1 l)))]
        ;; Alt nodes union the results from their children
        [alt-tag (append (parse-tree (node-child1 l)) (parse-tree (node-child2 l)))]
        ;; Seq nodes construct pairs of elements from their children
        [seq-tag (for+/list ([t1 (parse-tree (node-child1 l))]
                             [t2 (parse-tree (node-child2 l))])
                   (cons t1 t2))]
        [else (printf "parse-tree: unknown node-tag: ~s~n" (node-tag l))]))
    (node-value-set! l result) ;; Overwrite the temporary list of parse trees in our cache with the true value
    result]))

;; NOTE: The nullable check is important for correctness and not just performance.
;; (An early version of this code omitted that check, and the result where ... strange.)
;;
;; To see why, consider two nodes A and B respectively defined as (alt (seq B empty) eps) and (red A).
;; If parse-tree starts at A, then when it cycles back to B, it will see that there is a cycle.
;; Since we omit cycles, B will be marked as having no parse trees which is
;; incorrect as A generates a parse tree due to it's "eps" child.
;;
;; The key is that cycles should not be present in actual parse trees, but the seq in A
;; is not part of any actual parse tree due to its empty child.

;;;;;;;;;;;;;;;; GraphVis ;;;;;;;;;;;;;;;;

;; Ugh, this code is messy and not well documented.  Good luck!

(module+ graphvis
  (provide dotify)
  ;; Gives every object a unique value:
  (define mark-of-beast
    (let* ([index (make-hasheq)]
           [max   0]
           [next  (lambda ()
                    (set! max (+ max 1))
                    max)])
      (lambda (object)
        (if (hash-has-key? index object)
            (hash-ref index object)
            (begin
              (hash-set! index object (next))
              (mark-of-beast object))))))

; Allows recursive functions on graphs by
; turning them into graph searches:
(define-syntax define/search
  (syntax-rules ()
    [(_ (f x rest ...) #:reentry default body ...)
     ; =>
     (define f (let ([visited (make-parameter #f)]
                     [$def    default])
                 (lambda (x rest ...)
                   (cond
                     [(not (visited))
                      ; =>
                      (parameterize ([visited (make-hasheq)])
                        (f x rest ...))]

                     [(hash-has-key? (visited) x)
                      ; =>
                      (if (procedure? $def) ($def x) $def)]

                     [else
                      ; =>
                      (hash-set! (visited) x #t)
                      (let () body ...)]))))]

    [(_ (f x rest ...) body ...)
     ; =>
     (define/search (f x rest ...) #:reentry (lambda (x) (void)) body ...)]))

  ;; Outputs a grammar as a dot file.
  (define (dotify l #:port [port (current-output-port)])
    (define/search (dotify-nodes l depth port)
    (define color (if (nullable? l) "color = \"red\", " (if (eq? #f (node-nullable l)) "color = \"blue\", " "")))
    (define (f x) (let ([y (format "~s" (format "~s" x))]) (substring y 1 (- (string-length y) 1))))
      (define m (mark-of-beast l))
      (when (< depth 20) #;(hash-has-key? derive-touches l)
      (case-node-tag (node-tag l)
        [empty-tag (display (format "\"~s\" [~alabel = \"empty\"~n];~n~n" m color) port)]
        [eps-tag (display (format "\"~s\" [shape = \"record\", ~alabel = \"eps | ~a\"~n];~n~n" m color (f (force (node-child1 l)))) port)]
        [token-tag (display (format "\"~s\" [shape = \"record\", ~alabel = \"token | ~a\"~n];~n~n" m color (f (node-child2 l))) port)]
        [alt-tag
         (define l1 (node-child1 l))
         (define l2 (node-child2 l))
         (define m1 (mark-of-beast l1))
         (define m2 (mark-of-beast l2))
         (display (format "\"~s\" [~alabel = \"or\"~n];~n~n" m color) port)
         (dotify-nodes l1 (+ 1 depth) port)
         (dotify-nodes l2 (+ 1 depth) port)
         (display (format "\"~s\" -> \"~s\" [~n];~n~n" m m1) port)
         (display (format "\"~s\" -> \"~s\" [~n];~n~n" m m2) port)]
        [seq-tag
         (let ([origl l])
         (define l (node-child1 origl))
         (define r (node-child2 origl))
         (define ml (mark-of-beast l))
         (define mr (mark-of-beast r))
         (display (format "\"~s\" [shape=\"none\", margin=0, ~alabel = <~n<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>~n];~n~n" m color) port)
         (dotify-nodes l (+ 1 depth) port)
         (dotify-nodes r (+ 1 depth) port)
         (display (format "\"~s\":L -> \"~s\" [~n];~n~n" m ml) port)
         (display (format "\"~s\":R -> \"~s\" [~n];~n~n" m mr) port))]
        [red-tag
         (let ([origl l])
           (define l (node-child1 origl))
           (define ml (mark-of-beast l))
           (display (format "\"~s\" [shape = \"diamond\", ~alabel = \"red\"~n];~n~n" m color) port)
           (dotify-nodes l (+ 1 depth) port)
           (display (format "\"~s\" -> \"~s\" [~n];~n~n" m ml) port))]
        [else (printf "error:~s~n" l)]

      #;[(redp l f)
       ; =>
       (define ml (mark-of-beast l))
       (display (format "\"~s\" [label = \"red\"~n];~n~n" m) port)
       (dotify-nodes (+ 1 depth) l port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m ml) port)])))

    (define close-port? #f)

    (when (string? port)
      (set! close-port? #t)
      (set! port (open-output-file port #:mode 'text #:exists 'replace)))

    (display (format "digraph {~n~n") port)
    (display (format "node [];~n") port)
    (dotify-nodes l 0 port)
    (display (format "\"~s\" [shape = \"doublecircle\"~n];~n" (mark-of-beast l)) port)
    (display (format "~n}~n") port)

    (when close-port?
          (close-output-port port)))
)




;;;;;;;;;;;;;;;;;;;;;;;; Convenience Functions ;;;;;;;;;;;;;;;

(define empty (void))
(define eps (void))
(define eps* (void))
(define token (void))
(define alt (void))
(define seq (void))
(define rep (void))
(define $$ (void))
(define @--> (void))
(define >--> (void))
(define $--> (void))

; A language for languages:
(define-syntax (lang stx)
  (syntax-case stx (empty eps eps* quote ? token or rep rep+ opt seq seq* seq! quasiquote --> $--> @--> >--> car)
    [(_)                  #'(make-empty-node)]
    [(_ (empty))          #'(make-empty-node)]
    [(_ (eps))            #'(make-eps-node (delay (list #f)))]
    [(_ (eps v))          #'(make-eps-node (delay (list v)))]
    [(_ (eps* s))         #'(make-eps-node (delay s))]
    [(_ (? pred class))   #'(make-token-node pred class)]
    [(_ (token pred class)) #'(make-token-node pred class)]
    [(f (quote lit))      (with-syntax ([literal->language (datum->syntax #'f 'literal->language)])
                            #'(literal->language 'lit))]

    [(_ (or))             #'(make-empty-node)]
    [(f (or l1))          #'(f l1)]
    [(f (or l1 l2 ...))   #'(make-alt-node (f l1) (f (or l2 ...)))]

    [(_ (seq))            #'(make-eps-node (delay (list '())))]
;    [(f (seq l1))         #'(f l1)]
    [(f (seq l1 l2 ...))  #'(make-seq-node (f l1) (f (seq l2 ...)))]

    [(_ (seq*))           #'(make-eps-node (delay (list '())))]
    [(f (seq* l1))        #'(make-red-node (f l1) (lift-red (λ (w1) (list w1))))]
    [(f (seq* l1 l2 ...)) #'(make-seq-node (f l1) (f (seq* l2 ...)))]

    [(_ (seq!))            #'(make-eps-node (delay (list '())))]
    [(f (seq! `l1 l2 ...)) #'(lang (seq (lang l1) (seq! l2 ...)))]
    [(f (seq!  l1 l2 ...)) #'(lang (--> (seq (lang l1) (seq! l2 ...))
                                        (λ (res) (cdr res))))]

    [(f (rep l))          #'(shared ([x (make-alt-node (f (eps* (list '()))) (make-seq-node (f l) x))]) x)]

    [(f (rep+ l))         #'(make-seq-node (f l) (f (rep l)))]

    [(f (opt l))          #'(make-alt-node (f l) (f (eps* (list #f))))]
    [(f (opt l v))        #'(make-alt-node (f l) (f (eps* (list v))))]

    [(f (car l))          #'(make-red-node (f l) (lift-red (λ (r) (car r))))]

    [(f (-->  l g))       #'(make-red-node (f l) (lift-red g))]
    [(f (@--> l g))       #'(make-red-node (f l) (lift-red (λ (w) (apply g w))))]
    [(f (>--> l c ...))   #'(make-red-node (f l) (lift-red (λ (w) (match w c ...))))]
    [(f ($--> l e ...))   (with-syntax ([$  (datum->syntax #'l '$)]
                                        [$$ (datum->syntax #'l '$$)])
                            #'(make-red-node (f l)
                                   (lift-red (λ ($$)
                                     (let (($ (λ (n) (list-ref $$ (- n 1)))))
                                       e ...)))))]

    [(f atom)             (with-syntax ([literal->language (datum->syntax #'atom 'literal->language)])
                            (let ((d (syntax->datum #'atom)))
                              (cond
                                [(string? d)   #'(literal->language atom)]
                                [(number? d)   #'(literal->language atom)]
                                [(boolean? d)  #'(literal->language atom)]
                                [else          #'atom])))]

    [else                 (error "syntax error in lang")]))

(define (lift-red f) (lambda (x) (for/list ([t x]) (f t))))

; Specifies the default behavior for literals in the grammar:
(define (default-literal->language lit)
  (make-token-node (lambda (t) (equal? t lit)) lit))

(define literal->language default-literal->language)

; Set the behavior for literals in the grammar:
(define (set-literal->language! f)
  (set! literal->language f))

; Tools for defining grammars:
(define-syntax grammar
  (syntax-rules ()
    [(_)   (void)]
    [(_ (lhs rhs) ... body)
     ; =>
     (shared
      ([lhs (lang rhs)] ...)
      body)]))

; Allows a grammar from an external file:
(define-syntax (grammar-from-file stx)
  (syntax-case stx ()
    [(_ start filename)
     (let ()
       (define (read-all port)
         (let ([next (read port)])
           (if (eof-object? next)
               '()
               (cons next (read-all port)))))
       (let* ([filename (syntax->datum #'filename)]
              [contents (read-all (open-input-file filename))])
         (datum->syntax #'start `(grammar ,@contents ,(syntax->datum #'start)))))]))

(define-syntax (grammar-from-cfg-file stx)
  (syntax-case stx ()
    [(_ start filename)
     (let ()
       (define (read-all port)
         (let ([next (read port)])
           (if (eof-object? next)
               '()
               (cons next (read-all port)))))
       (let* ([filename (syntax->datum #'filename)]
              [contents (read-all (open-input-file filename))]
              [lhs* (map car contents)])
         (define (atom->derp x)
           (define s (symbol->string x))
           (cond
            [(andmap char-upper-case? (string->list s)) `',x]
            [(not (member x lhs*)) s]
            [else x]))

         (define (code->rule x)
           (caddr (caddr x)))

         (define (rhs->derp x)
           (define rhs (car x))
           (define code (cadr x))
           `($--> (seq ,@(map atom->derp rhs)) ,(code->rule code)))

         (define (prod->derp x)
           (define lhs (car x))
           (define rhs* (cdr x))
           `(,lhs (or ,@(map rhs->derp rhs*))))

         (define (cfg->derp x)
           (datum->syntax #'start `(grammar ,@(map prod->derp contents) ,(syntax->datum #'start))))

         (cfg->derp contents)))]))

; Read all lines from a file:
(define (read-all port)
  (let ((next (read port)))
    (if (eof-object? next)
        '()
        (cons next (read-all port)))))
