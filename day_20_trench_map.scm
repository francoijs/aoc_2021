;; https://adventofcode.com/2021/day/20

(load "utils.scm")

(define (read-image-enhancement lines)
  (define (char->bool c) (eq? c #\#))
  (let ((algo-vec (list->vector (map char->bool (string->list (car lines)))))
        (image (matrix-map char->bool (read-matrix (cddr lines) identity))))
    (assert (= (expt 2 9) (vector-length algo-vec)))
    (values algo-vec image)))

(define *test-enhancer* '("..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
                          ""
                          "#..#."
                          "#...."
                          "##..#"
                          "..#.."
                          "..###"))
(define *test-algo* (let-values (((algo _) (read-image-enhancement *test-enhancer*))) algo))
(define *test-image* (let-values (((_ img) (read-image-enhancement *test-enhancer*))) img))

(define (bool->char b) (if b #\# #\.))
(define (display-block n)
  (let* ((bs (unsigned-integer->bit-string 9 n))
         (1st (bit-substring bs 6 9))   ; hi index = leftmost bits
         (2nd (bit-substring bs 3 6))
         (3rd (bit-substring bs 0 3)))  ; low index  = rightmost bits
    (newline)
    (display (list->string (bit-string-map bool->char 1st)))(newline)
    (display (list->string (bit-string-map bool->char 2nd)))(newline)
    (display (list->string (bit-string-map bool->char 3rd)))(newline)))
(display-block (+ 4 16 64))   ; shall display '/'

;; add a border of #t around the image (margin on each side)
(define (enlarge-image mat margin)
  (let* ((h (matrix-height mat))
         (w (matrix-width mat))
         (new-mat (make-matrix (+ h (* 2 margin))
                               (+ w (* 2 margin)) #f)))
    (submatrix-copy! mat 0 0 h w new-mat margin margin)
    new-mat))
(assert (equal? '(16 . 26) (matrix-dimensions (enlarge-image (make-matrix 10 20 #t) 3))))
;; (display-bit-matrix (enlarge-image (make-matrix 3 3 #t) 1 1))

(define (crop-image img margin)
  (submatrix img margin margin
             (- (matrix-height img) (* 2 margin))
             (- (matrix-width img) (* 2 margin))))
(matrix-dimensions (crop-image (enlarge-image *test-image* 3) 3))

(define (square->integer img row col)
  (assert (< 0 row (-1+ (matrix-height img))))
  (assert (< 0 col (-1+ (matrix-width img))))
  (let ((bs (make-bit-string 9 #f)))
    (matrix-for-each (lambda (row col b)
                       (if b (bit-string-set! bs (- 8 (* 3 row) col))))
                     (submatrix img (-1+ row) (-1+ col) 3 3))
    (bit-string->unsigned-integer bs)))
(assert (= 34 (square->integer *test-image* 2 2)))

(define (enhance-image algo img margin)
  (let* ((img (enlarge-image img margin))
         (w (matrix-width img))
         (h (matrix-height img))
;         (algo (vector-set (vector-set algo 0 #t) 511 #f))
         (new-img (make-matrix h w (and (vector-ref algo 0)
                                        (matrix-ref img 0 0)))))
    (matrix-for-each (lambda (y x _)
                       (matrix-set! new-img (1+ y) (1+ x)
                                    (vector-ref algo (square->integer img (1+ y) (1+ x)))))
                     (submatrix img 1 1 (- h 2) (- w 2)))
    new-img))
;(display-bit-matrix *test-image*)
;(display-bit-matrix (enhance-image *test-algo* *test-image* 3))

(define (count-lit-pixels img) (matrix-count img identity))
(assert (= 35 (count-lit-pixels (enhance-image *test-algo* (enhance-image *test-algo* *test-image* 2) 2))))

;; part 1
(let-values (((algo img) (read-image-enhancement (load-file "day_20_input.txt"))))
  (assert (= 5619 (count-lit-pixels
                   (crop-image
                    (enhance-image algo
                                   (enhance-image algo img 4) 0) 2)))))


(define (image-enhancement/step algo img n)
  (fold (lambda (_ img) (enhance-image algo img 2))
        img
        (iota n)))
(assert (= 35 (count-lit-pixels (image-enhancement/step *test-algo* *test-image* 2))))
(assert (= 3351 (count-lit-pixels (image-enhancement/step *test-algo* *test-image* 50))))



;; (define (square->tuple img row col)
;;   (let loop ((y (-1+ row))
;;              (x (-1+ col))
;;              (tuple '()))
;;     (cond ((= y (+ 2 row)) tuple)
;;           ((= x (+ 2 col)) (loop (1+ y) (-1+ col) tuple))
;;           (else
;;            (loop y (1+ x) (cons (square->integer img y x) tuple))))))
;; (assert (= 9 (length (square->tuple *test-image* 2 2))))
;; (assert (= 34 (fifth (square->tuple *test-image* 2 2))))

;; ;; 9 squares per pixel -> tuple of 9 integers
;; (define (image-tuples-distribution img)
;;   (assert (> (matrix-width img) 4))
;;   (assert (> (matrix-height img) 4))
;;   (let ((distri (make-equal-hash-table))
;;         (w (matrix-width img))
;;         (h (matrix-height img)))
;;     (matrix-for-each (lambda (y x _)
;;                        (warn y x h w)
;;                        (hash-table/inc! distri (square->tuple img (+ 2 y) (+ 2 x))))
;;                      (submatrix img 1 1 (- h 2) (- w 2)))
;;     distri))

;; (image-tuples-distribution (enlarge-image *test-image* 2 2))
