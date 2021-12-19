;; https://adventofcode.com/2021/day/18

(load "utils.scm")

;; each node is (node-left node-right #f)
(define (read-pair-tree str)
  (let ((bstr (string-append str "*")))
    (read (open-input-string
           ((string-trimmer 'to-trim #\*)
            (apply (string-joiner 'infix " #f)")
                   ((string-splitter 'delimiter #\] 'allow-runs? #f)
                    (string-replace
                     (string-replace bstr #\[ #\( )
                     #\, #\space ))))))))
(assert (equal? '(1 (2 3 #f) #f) (read-pair-tree "[1,[2,3]]")))
(assert (equal? '(1 ((2 4 #f) 3 #f) #f) (read-pair-tree "[1,[[2,4],3]]")))

;; replace #f with ref to the parent in the tree (except top node)
(define (set-parents! tree)
  (define (recurse node parent)
    (if (list? node)
        (begin
          (set-cdr! (cdr node) (list parent))
          (recurse (first node) node)
          (recurse (second node) node))))
  (recurse tree #f))

(define (string->tree str)
  (define tree (read-pair-tree str))
  (set-parents! tree)
  tree)

(define (tree->string tree)
  (let ((left (first tree))
        (right (second tree)))
    ((string-joiner)
     "[" (if (list? left)
             (tree->string left)
             (number->string left))
     "," (if (list? right)
             (tree->string right)
             (number->string right)) "]")))
(map (lambda (s) (string=? s (tree->string (string->tree s))))
     '("[[[[[9,8],1],2],3],4]"
       "[7,[6,[5,[4,[3,2]]]]]"
       "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))

(define (find-top node dir)
  (let ((parent (third node)))
    (assert (or (not parent) (eq? (first parent) node) (eq? (second parent) node)))
    (cond ((not parent) #f)
          ((eq? node (dir parent)) parent)
          (else (find-top parent dir)))))
(define find-top-left (lambda (node) (find-top node second)))
(assert (not (find-top-left (caaaar (string->tree "[[[[[9,8],1],2],3],4]")))))
(assert (find-top-left (second (string->tree "[4,[3,2]]"))))
(define find-top-right (lambda (node) (find-top node first)))
(assert (not (find-top-right (second (string->tree "[4,[3,2]]")))))
(assert (find-top-right (second (second (first (string->tree "[[6,[5,[4,[3,2]]]],1]"))))))

(define (find-number-pair node num)
  (let ((left (first node))
        (right (second node)))
    (cond ((or (eq? num left) (eq? num right)) node)
          ((and (list? left) (find-number-pair left num)) (find-number-pair left num))
          ((and (list? right) (find-number-pair right num)) (find-number-pair right num))
          (else #f))))

(define (find-pair node dir)
  (cond ((not node) #f)
        ((number? (dir node)) node)
        (else (find-pair (dir node) dir))))

(define (find-left-pair node)
  (let ((top-left (find-top-left node)))
    (cond ((not top-left) #f)
          ((number? (first top-left)) top-left)
          (else (find-pair (first top-left) second)))))
(assert (not (find-left-pair (find-number-pair (string->tree "[[[[[9,8],1],2],3],4]") 9))))
(assert (string=? "[4,[3,2]]" (tree->string (find-left-pair (find-number-pair (string->tree "[7,[6,[5,[4,[3,2]]]]]") 3)))))
(assert (string=? "[4,[3,2]]" (tree->string (find-left-pair (find-number-pair (string->tree "[[6,[5,[4,[3,2]]]],1]") 3)))))
(assert (string=? "[1,[7,3]]" (tree->string (find-left-pair (find-number-pair (string->tree "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") 7)))))
(assert (string=? "[4,[1,2]]" (tree->string (find-left-pair (find-number-pair (string->tree "[[3,[2,[8,0]]],[9,[5,[4,[1,2]]]]]") 1)))))

(define (find-right-pair node)
  (let ((top-right (find-top-right node)))
    (cond ((not top-right) #f)
          ((number? (second top-right)) top-right)
          (else (find-pair (second top-right) first)))))
(assert (string=? "[[9,8],1]" (tree->string (find-right-pair (find-number-pair (string->tree "[[[[[9,8],1],2],3],4]") 9)))))
(assert (not (find-right-pair (find-number-pair (string->tree "[7,[6,[5,[4,[3,2]]]]]") 3))))
(assert (string=? "[[6,[5,[4,[3,2]]]],1]" (tree->string (find-right-pair (find-number-pair (string->tree "[[6,[5,[4,[3,2]]]],1]") 3)))))
(assert (string=? "[6,[5,[4,[3,2]]]]" (tree->string (find-right-pair (find-number-pair (string->tree "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") 7)))))
(assert (not (find-right-pair (find-number-pair (string->tree "[[3,[2,[8,0]]],[9,[5,[4,[1,2]]]]]") 1))))

(define (inc-pair! pair num pref set-pref)
  (cond ((and (number? (first pair)) (number? (second pair))) (set-pref pair (+ num (pref pair))))
        ((number? (first pair)) (set-first! pair (+ num (first pair))))
        ((number? (second pair)) (set-second! pair (+ num (second pair))))
        (else
         ("cannot increment" (tree->string pair)))))

;; return #f if not explode were performed
(define (reduce-explode! tree)
  (call/cc
   (lambda (return)
     (let loop ((node tree) (depth 0))
       (let ((left (first node))
             (right (second node))
             (parent (third node)))
         (if (< depth 4)
             (begin
               (if (list? left)
                   (loop left (1+ depth)))
               (if (list? right)
                   (loop right (1+ depth))))
             (begin
               (assert (number? left))
               (assert (number? right))
               (let ((left-pair (find-left-pair node))
                     (right-pair (find-right-pair node)))
                 (if left-pair
                     (begin
;;                       (warn "explode" left "to the left" (tree->string left-pair))
                       (inc-pair! left-pair left second set-second!)))
                 (if right-pair
                     (begin
;;                       (warn "explode" right "to the right" (tree->string right-pair))
                       (inc-pair! right-pair right first set-first!)))
                 (if (eq? node (first parent))
                     (set-first! parent 0))
                 (if (eq? node (second parent))
                     (set-second! parent 0))
                 (return tree))))))
     (return #f))))
(assert (string=? "[[[[0,9],2],3],4]" (tree->string (reduce-explode! (string->tree "[[[[[9,8],1],2],3],4]")))))
(assert (string=? "[7,[6,[5,[7,0]]]]" (tree->string (reduce-explode! (string->tree "[7,[6,[5,[4,[3,2]]]]]")))))
(assert (string=? "[[6,[5,[7,0]]],3]" (tree->string (reduce-explode! (string->tree "[[6,[5,[4,[3,2]]]],1]")))))
(assert (string=? "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" (tree->string (reduce-explode! (string->tree "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")))))
(assert (string=? "[[3,[2,[8,0]]],[9,[5,[7,0]]]]" (tree->string (reduce-explode! (string->tree "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))))
(assert (string=? "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" (tree->string (reduce-explode! (string->tree "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))))
(assert (string=? "[[[[0,7],4],[15,[0,13]]],[1,1]]" (tree->string (reduce-explode! (string->tree "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")))))
(assert (string=? "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" (tree->string (reduce-explode! (string->tree "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")))))

;; return #f if not split were performed
(define (reduce-split! tree)
  (call/cc
   (lambda (return)
     (let loop ((node tree) (depth 0))
       (let ((left (first node))
             (right (second node))
             (parent (third node)))
         (begin
           (if (list? left)
               (loop left (1+ depth))
               (if (> left 9)
                   (begin
                     (set-first! node (list (floor (/ left 2))
                                            (ceiling (/ left 2))
                                            node))
                     (return tree))))
           (if (list? right)
               (loop right (1+ depth))
               (if (> right 9)
                   (begin
                     (set-second! node (list (floor (/ right 2))
                                             (ceiling (/ right 2))
                                             node))
                     (return tree)))))))
     (return #f))))
(assert (string=? "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" (tree->string (reduce-split! (string->tree "[[[[0,7],4],[15,[0,13]]],[1,1]]")))))
(assert (string=? "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]" (tree->string (reduce-split! (string->tree "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")))))

(define (tree-reduce! tree)
  (let ((tree1 (reduce-explode! tree)))
    (if tree1
        (tree-reduce! tree1)
        (let ((tree2 (reduce-split! tree)))
          (if tree2
              (tree-reduce! tree2)
              tree)))))
(assert (string=? "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" (tree->string (tree-reduce! (string->tree "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))))

(define (tree-add! t1 t2)
  (let ((res (list t1 t2 #f)))
    (set-third! t1 res)
    (set-third! t2 res)
    res))
(assert (string=? "[[1,2],[[3,4],5]]" (tree->string (tree-add! (string->tree "[1,2]") (string->tree "[[3,4],5]")))))
(assert (string=? "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" (tree->string (tree-add! (string->tree "[[[[4,3],4],4],[7,[[8,4],9]]]") (string->tree "[1,1]")))))

(define (snailfish-homework trees)
  (fold (lambda (tree sum) (tree-reduce! (tree-add! sum tree)))
        (car trees)
        (cdr trees)))
(assert (string=? "[[[[1,1],[2,2]],[3,3]],[4,4]]"
                  (tree->string (snailfish-homework (map string->tree '("[1,1]"
                                                                        "[2,2]"
                                                                        "[3,3]"
                                                                        "[4,4]"))))))
(assert (string=? "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
                  (tree->string (snailfish-homework (map string->tree '("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                                                                        "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                                                                        "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                                                                        "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                                                                        "[7,[5,[[3,8],[1,4]]]]"
                                                                        "[[2,[2,2]],[8,[8,1]]]"
                                                                        "[2,9]"
                                                                        "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                                                                        "[[[5,[7,4]],7],1]"
                                                                        "[[[[4,2],2],6],[8,7]]"))))))
(assert (string=? "[[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]]"
                  (tree->string (snailfish-homework (map string->tree '("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                                                                        "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"))))))

(define (magnitude node)
  (if (number? node)
      node
      (let ((left (first node))
            (right (second node)))
        (+ (* 3 (magnitude left))
           (* 2 (magnitude right))))))
(assert (= 29 (magnitude (string->tree "[9,1]"))))
(assert (= 129 (magnitude (string->tree "[[9,1],[1,9]]"))))
(assert (= 3488 (magnitude (string->tree "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))))

(define *homework*  '("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
                      "[[[5,[2,8]],4],[5,[[9,9],0]]]"
                      "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
                      "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
                      "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
                      "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
                      "[[[[5,4],[7,7]],8],[[8,3],8]]"
                      "[[9,3],[[9,9],[6,[4,9]]]]"
                      "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                      "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"))
(assert (= 4140 (magnitude (snailfish-homework (map string->tree *homework*)))))

;; part 1
(assert (= 2501 (chain (map string->tree (load-file "day_18_input.txt"))
                       snailfish-homework
                       magnitude)))


(define (find-max-magnitude lines)
  (fold (lambda (str1 best1)
          (max best1 (fold (lambda (str2 best2)
                             (if (string=? str1 str2)
                                 best2
                                 (max best2 (chain (tree-add! (string->tree str1)
                                                              (string->tree str2))
                                                   tree-reduce!
                                                   magnitude))))
                           0
                           lines)))
        0
        lines))
(assert (= 3993 (find-max-magnitude *homework*)))

;; part 2
(assert (= 4935 (chain (load-file "day_18_input.txt")
                       find-max-magnitude)))
