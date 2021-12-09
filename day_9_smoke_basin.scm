;; https://adventofcode.com/2021/day/9

(load "utils.scm")

(define (read-heightmap lines)
  (let* ((matrix
          (let loop((lines lines) (row 1) (matrix (make-vector 1 #f)))
            (if (null? lines) (vector-grow-set! matrix row #f)
                (loop (cdr lines) (1+ row)
                      (vector-grow-set! matrix row
                                        (list->vector (append '(9)
                                                              (map (lambda (c) (- (char->integer c)
                                                                                  (char->integer #\0)))
                                                                   (string->list (car lines)))
                                                              '(9))))))))
         (cols (vector-length (vector-ref matrix 1)))
         (rows (matrix-height matrix)))
    (vector-set! matrix 0 (make-vector cols 9))
    (vector-set! matrix (-1+ rows) (make-vector cols 9))
    matrix))

(define (low-point? matrix row col)
  (let ((level (matrix-ref matrix row col)))
    (and (< level (matrix-ref matrix (-1+ row) col))
         (< level (matrix-ref matrix (1+ row) col))
         (< level (matrix-ref matrix row (-1+ col)))
         (< level (matrix-ref matrix row (1+ col))))))

(define *test-map* (read-heightmap '("2199943210"
                                     "3987894921"
                                     "9856789892"
                                     "8767896789"
                                     "9899965678"
                                     )))
(assert (= 9 (matrix-ref *test-map* 0 0)))
(assert (= 2 (matrix-ref *test-map* 1 1)))
(assert (= 8 (matrix-ref *test-map* 5 10)))
(assert (= 9 (matrix-ref *test-map* 6 11)))
(assert (low-point? *test-map* 1 2))

(define-generator (low-points-iterator matrix)
  (let ((h (- (matrix-height matrix) 1))
        (w (- (matrix-width matrix) 1)))
    (warn h w)
    (do ((row 1 (1+ row)))
        ((= row h) (yield #f))
      (do ((col 1 (1+ col)))
          ((= col w))
        (if (low-point? matrix row col)
            (yield (list (1+ (matrix-ref matrix row col)) row col)))))))
(define (find-risk-levels matrix)
  (map first (iterator->list (low-points-iterator matrix))))
(assert (= 15 (apply + (find-risk-levels *test-map*))))


;; part 1
(assert (= 458 (apply + (find-risk-levels (read-heightmap (load-file "day_9_input.txt"))))))


;; part 2
(define (count-neighbors! matrix row col)
  (let ((height (matrix-ref matrix row col)))
;;    (warn row col height)
    (if (= 9 height) 0
        (begin
          (matrix-set! matrix row col 9)
          (+ 1 
             (count-neighbors! matrix (-1+ row) col)
             (count-neighbors! matrix (1+ row) col)
             (count-neighbors! matrix row (-1+ col))
             (count-neighbors! matrix row (1+ col)))))))
(assert (= 3 (count-neighbors! *test-map* 1 2)))
(assert (= 14 (count-neighbors! *test-map* 3 3)))

(let ((hmap (read-heightmap (load-file "day_9_input.txt"))))
  (assert (= 1391940 (apply * (list-head (sort (map (lambda (lp) (count-neighbors! hmap (second lp) (third lp)))
                                                    (iterator->list (low-points-iterator hmap)))
                                               >)
                                         3)))))
