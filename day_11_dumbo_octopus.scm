;; https://adventofcode.com/2021/day/11

(load "utils.scm")

(define (read-octopus-grid lines)
  (read-matrix-add-border lines #f))

(define (neighbors row col)
  (let ((deltas '(-1 0 1)))
    (fold (lambda (drow ls)
            (append ls (fold (lambda (dcol ls)
                               (if (= drow dcol 0) ls
                                   (cons (cons (+ row drow)
                                               (+ col dcol)) ls)))
                             '() deltas)))
          '() deltas)))
(assert (equal? '((2 . 4) (2 . 3) (2 . 2) (3 . 4) (3 . 2) (4 . 4) (4 . 3) (4 . 2))
                (neighbors 3 3)))

;; perform 1 step and return the count of flashes
(define (octopus-grid/step! grid)
  (let ((flashes 0)
        (flashed (matrix-map not grid)))
    (define (update-level coord)
      (let ((row (car coord)) (col (cdr coord)))
        (if (not (matrix-ref grid row col)) 0
            (begin
              (unless (matrix-ref flashed row col)
                (matrix-inc! grid row col))
              (if (> (matrix-ref grid row col) 9)
                  (begin
                    (matrix-set! grid row col 0)
                    (matrix-set! flashed row col #t)
                    (fold (lambda (neigh count) (+ count (update-level neigh)))
                          1
                          (neighbors row col)))
                  0)))))
    (matrix-for-each (lambda (row col lvl)
                       (set! flashes (+ flashes (update-level (cons row col)))))
                     grid)
    flashes))

(define *test-grid* (read-octopus-grid '("11111" "19991" "19191" "19991" "11111")))
(assert (= 9 (octopus-grid/step! *test-grid*)))
(assert (= 0 (octopus-grid/step! *test-grid*)))
(assert (= 6 (matrix-ref *test-grid* 3 5)))
(assert (= 5 (matrix-ref *test-grid* 4 1)))

(define (iterate-grid grid steps)
  (fold (lambda (_ flashes) (+ flashes (octopus-grid/step! grid)))
        0 (iota steps)))
(define *test-lines* '("5483143223"
                       "2745854711"
                       "5264556173"
                       "6141336146"
                       "6357385478"
                       "4167524645"
                       "2176841721"
                       "6882881134"
                       "4846848554"
                       "5283751526"))
(assert (= 204 (iterate-grid (read-octopus-grid *test-lines*) 10)))
(assert (= 1656 (iterate-grid (read-octopus-grid *test-lines*) 100)))

;; part 1
(assert (= 1655 (iterate-grid (read-octopus-grid (load-file "day_11_input.txt")) 100)))


(define (step-until-simultaneous-flashes! grid max-steps)
  (call/cc
   (lambda (return)
     (let loop ((step 1))
       (cond
        ((= max-steps step) (error "not found!"))
        ((= (octopus-grid/step! grid) (* (- (matrix-width grid) 2)
                                         (- (matrix-height grid) 2)))
         (return step))
        (else
         (loop (1+ step))))))))
(assert (= 195 (step-until-simultaneous-flashes! (read-octopus-grid *test-lines*) 200)))

;; part 2
(assert (= 337 (step-until-simultaneous-flashes! (read-octopus-grid (load-file "day_11_input.txt")) 1000)))
