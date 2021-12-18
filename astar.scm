;; Port from the Racket version at:
;; https://jeapostrophe.github.io/2013-04-15-astar-post.html

(load "utils.scm")
(load-option 'format)

(define-record-type :map-node
  (make-map-node M x y)
  map-node?
  (M map-node:M)
  (x map-node:x)
  (y map-node:y))
(define (display-node n)
  (format #t "[~A,~A:~A]"
          (map-node:x n) (map-node:y n)
          (matrix-ref (map-node:M n) (map-node:x n) (map-node:y n))))

(define-record-type :map-edge
  (make-map-edge src dst dx dy)
  map-edge?
  (src map-edge:src)
  (dst map-edge:dst)
  (dx map-edge:dx)
  (dy map-edge:dy))
(define (display-edge n)
  (format #t "[~A,~A->~A,~A]"
          (map-node:x (map-edge:src n)) (map-node:y (map-edge:src n))
          (map-node:x (map-edge:dst n)) (map-node:y (map-edge:dst n))))

(define (xor a b) (and (or a b) (not (and a b))))

;; no diagonals
(define (node-edges n)
  (assert (map-node? n))
  (let ((m (map-node:M n))
        (x (map-node:x n))
        (y (map-node:y n)))
    (let loop ((dx '(-1 0 1)) (dy '(-1 0 1)) (ls '()))
      (cond ((null? dy) ls)
            ((null? dx) (loop '(-1 0 1) (cdr dy) ls))
            (else
             (loop (cdr dx) dy
                   (let ((dx (car dx))
                         (dy (car dy)))
                     (if (and (xor (zero? dx) (zero? dy))
                              (<= 0 (+ x dx) (-1+ (matrix-width m)))
                              (<= 0 (+ y dy) (-1+ (matrix-height m))))
                         (cons (make-map-edge n (make-map-node m (+ x dx) (+ y dy))
                                              dx dy) ls)
                         ls))))))))

;; priority queue
(define-record-type :heap
  (make-heap-impl cmp ls)
  heap?
  (cmp heap:cmp)
  (ls heap:ls heap:set-ls!))
(define (make-heap cmp)
  (make-heap-impl cmp (make-list 0)))
(define (heap/add! h obj)
  (heap:set-ls! h (sort (cons obj (heap:ls h)) (heap:cmp h))))
(define (heap/consume! h)
  (if (null? (heap:ls h)) #f
      (let ((obj (car (heap:ls h))))
        (heap:set-ls! h (cdr (heap:ls h)))
        obj)))


(define (A* graph initial node-cost edge-cost)
  (call/cc
   (lambda (return)
     
     ;; closed set
     (define (node=? n1 n2)
       (and (= (map-node:x n1) (map-node:x n2))
            (= (map-node:y n1) (map-node:y n2))))
     (define (hash-node n . bound)
       (modulo (+ (* (map-node:y n) (matrix-width (map-node:M n))) (map-node:x n)) (car bound)))
     (let ((node->best-path (make-hash-table node=? hash-node))
           (node->best-path-cost (make-hash-table node=? hash-node)))
       (hash-table/put! node->best-path initial '())
       (hash-table/put! node->best-path-cost initial 0)

       ;; open set
       (define Inf 1000000)
       (define (node-total-estimate-cost n)
         (+ (node-cost n)
            (hash-table/get node->best-path-cost n #f)))
       (define (node-cmp x y)
         (<= (node-total-estimate-cost x)
             (node-total-estimate-cost y)))
       (let ((open-set (make-heap node-cmp))
             (count 0))
         (heap/add! open-set initial)

         (let a-star-loop ((x (heap/consume! open-set)))
;;           (display "open-set: ")(map display-node (heap:ls open-set))(newline)
           (set! count (1+ count))

           (let ((path-x (hash-table/get node->best-path x #f))
                 (g-x (hash-table/get node->best-path-cost x #f)))

             ;; a-star loop stop?
             (if (zero? (node-cost x))
                 (begin
                   (format #t "visited ~A nodes" count)
                   (return (reverse path-x))))
             
             ;; a-star loop body
             (let visit-neighbors ((x->y (node-edges x)))
               (unless (null? x->y)
               
                 ;; a-star loop per neighbor
                 (let* ((y (map-edge:dst (car x->y)))
                        (new-g-y (+ g-x (edge-cost (car x->y))))
                        (old-g-y (hash-table/get node->best-path-cost y Inf)))
                   
                   (if (< new-g-y old-g-y)
                       (begin
                         (hash-table/put! node->best-path-cost y new-g-y)
                         (hash-table/put! node->best-path y (cons (car x->y) path-x))
                         (heap/add! open-set y)))
                   (visit-neighbors (cdr x->y)))))
             
             (a-star-loop (heap/consume! open-set)))))))))


;; NxN grid where a random sample of cells are given the characteristic: plains (0), woods (1), or mountains (2).
(define (make-map N)
  (build-matrix N N (lambda (x y) (random 3))))
(define (display-map M)
  (newline)
  (vector-map (lambda (v) (vector-map display v)(newline)) M)
  #t)


;; It will cost 1 to move into plains, 5 to move into woods, and 10 to move into mountains.
(define (edge-cost e)
  (assert (map-edge? e))
  (let* ((n (map-edge:dst e))
         (cell (matrix-ref (map-node:M n) (map-node:x n) (map-node:y n))))
    (cond ((= cell 0) 1)
          ((= cell 1) 5)
          ((= cell 2) 10)
          (else
           (error "invalid cell" cell "at" (map-node:x n) (map-node:y n))))))

;; Manhattan distance
(define (make-node-cost GX GY)
  (lambda (n)
    (assert (map-node? n))
    (+ (abs (- (map-node:x n) GX))
       (abs (- (map-node:y n) GY)))))

(define N 10)
(define random-M (make-map N))
(display-map random-M)

(for-each display-edge
          (A* random-M
              (make-map-node random-M 0 0)
              (make-node-cost (-1+ N) (-1+ N))
              edge-cost))
