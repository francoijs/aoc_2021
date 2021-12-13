;; https://adventofcode.com/2021/day/13

(load "utils.scm")

(define-record-type :camera-manual
  (make-camera-manual paper instructions)
  camera-manual?
  (paper camera-manual:paper)
  (instructions camera-manual:instructions))

(define (read-camera-manual lines)
  (let* ((separator-index (list-index-of lines ""))
         (paper (fold (lambda (line matrix)
                        (let ((coords (map string->number (string-split line ","))))
                          (matrix-grow-set! matrix (second coords) (first coords) #t)))
                      (make-matrix 0 0  #f)
                      (list-head lines separator-index)))
         (instructions (fold (lambda (line ls)
                               (let ((tokens (string-split line " =")))
                                 (cons (cons (string->symbol (list-ref tokens 2))
                                             (string->number (list-ref tokens 3)))
                                       ls)))
                             '()
                             (reverse (list-tail lines (1+ separator-index))))))
    (make-camera-manual paper instructions)))

(define *test-manual* (read-camera-manual '("6,10"
                                            "0,14"
                                            "9,10"
                                            "0,3"
                                            "10,4"
                                            "4,11"
                                            "6,0"
                                            "6,12"
                                            "4,1"
                                            "0,13"
                                            "10,12"
                                            "3,4"
                                            "3,0"
                                            "8,4"
                                            "1,10"
                                            "2,14"
                                            "8,10"
                                            "9,0"
                                            ""
                                            "fold along y=7"
                                            "fold along x=5")))
(assert (= 11 (matrix-width (camera-manual:paper *test-manual*))))
(assert (= 15 (matrix-height (camera-manual:paper *test-manual*))))
(assert (matrix-ref (camera-manual:paper *test-manual*) 10 6))
(assert (not (matrix-ref (camera-manual:paper *test-manual*) 11 6)))
(assert (equal? '(y . 7) (first (camera-manual:instructions *test-manual*))))
(assert (equal? '(x . 5) (second (camera-manual:instructions *test-manual*))))

(define (fold-paper-horizontally! paper x)
  (assert (= x (quotient (matrix-width paper) 2)))
  (let* ((width (matrix-width paper))
         (right-start (if (odd? width) (+ 1 x) x))
         (left-part (submatrix paper 0 0 (matrix-height paper) x))
         (right-part (submatrix paper 0 right-start (matrix-height paper) x)))
    (begin
      (matrix-for-each (lambda (row col val) (if val (matrix-set! left-part row col val)))
                       (matrix-flip right-part 0))
      left-part)))
(assert (= 15 (matrix-height (fold-paper-horizontally! (camera-manual:paper *test-manual*) 5))))
(assert (= 5  (matrix-width (fold-paper-horizontally! (camera-manual:paper *test-manual*) 5))))

(define (fold-paper-vertically! paper y)
  (matrix-flip (fold-paper-horizontally! (matrix-flip paper 2) y) 2))
(assert (= 11 (matrix-width (fold-paper-vertically! (camera-manual:paper *test-manual*) 7))))
(assert (= 7  (matrix-height (fold-paper-vertically! (camera-manual:paper *test-manual*) 7))))

(define (fold-paper! paper instruction)
  (apply (if (equal? 'x (car instruction)) fold-paper-horizontally! fold-paper-vertically!)
         (list paper (cdr instruction))))
(assert (= 11 (matrix-width (fold-paper! (camera-manual:paper *test-manual*) '(y . 7)))))
(assert (= 7  (matrix-height (fold-paper! (camera-manual:paper *test-manual*) '(y . 7)))))

(define true? (lambda (b) b))
(assert (= 17 (matrix-count (fold-paper! (camera-manual:paper *test-manual*)
                                         (first (camera-manual:instructions *test-manual*)))
                            true?)))

;; part 1
(let ((manual (read-camera-manual (load-file "day_13_input.txt"))))
  (assert (= 785 (matrix-count (fold-paper! (camera-manual:paper manual)
                                            (first (camera-manual:instructions manual)))
                               true?))))


(define (follow-manual manual)
  (fold (lambda (instr paper)
          (begin (warn instr (matrix-width paper) (matrix-height paper))
                 (fold-paper! paper instr)))
        (camera-manual:paper manual)
        (camera-manual:instructions manual)))
(assert (= 16 (matrix-count (follow-manual *test-manual*) true?)))

(define (paper-display paper)
  (for-each (lambda (row) (begin
                            (vector-map (compose display (lambda (b) (if b "#" ".")))
                                        row)
                            (newline)))                    
            (vector->list paper)))
(chain *test-manual*
       follow-manual
       paper-display)

;; part 2   FIXME
(chain "day_13_input.txt"
       load-file
       read-camera-manual
       follow-manual
       paper-display)
