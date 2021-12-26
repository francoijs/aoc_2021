;; https://adventofcode.com/2021/day/25

(load "utils.scm")

(define (read-seafloor lines)
  (read-matrix lines
               (lambda (c) (case c
                             ((#\.) 0)
                             ((#\>) 1)
                             ((#\v) 2)))))
(define *test-seafloor* (read-seafloor '("...>..."
                                         "......."
                                         "......>"
                                         "v.....>"
                                         "......>"
                                         "......."
                                         "..vvv..")))
(define *test-seafloor-2* (read-seafloor '("v...>>.vv>"
                                           ".vv>>.vv.."
                                           ">>.>v>...v"
                                           ">>v>>.>.v."
                                           "v>v.vv.v.."
                                           ">.>>..v..."
                                           ".vv..>.>v."
                                           "v.v..>>v.v"
                                           "....v..v.>")))

;; try moving cc 1 from given position and return #t if success
(define (seafloor/move-east! sea new-sea row col)
  (let* ((w (matrix-width sea))
         (col1 (modulo (1+ col) w)))
    (if (zero? (matrix-ref sea row col1))
        (begin (matrix-set! new-sea row col1 1)
               #t)
        (begin (matrix-set! new-sea row col 1)
               #f))))

;; try moving cc 2 from given position and return #t if success
(define (seafloor/move-south! sea new-sea row col)
  (let* ((h (matrix-height sea))
         (row1 (modulo (1+ row) h)))
    (if (zero? (matrix-ref sea row1 col))
        (begin (matrix-set! new-sea row1 col 2)
               #t)
        (begin (matrix-set! new-sea row col 2)
               #f))))

;; move cucumbers and return (new state . number of moves)
(define (seafloor/step sea0)
  (define (move-cucumbers! sea typ proc)
    (let* ((w (matrix-width sea))
           (h (matrix-height sea))
           (new-sea (make-matrix h w 0))
           (count 0))
      (matrix-for-each
       (lambda (row col cc)
         (cond ((= cc typ)
                (if (proc sea new-sea row col)
                    (set! count (1+ count))))
               ((= cc (- 3 typ))
                (matrix-set! new-sea row col cc))))
       sea)
      (values new-sea count)))
  (let*-values (((sea1 count1) (move-cucumbers! sea0 1 seafloor/move-east!))
                ((sea2 count2) (move-cucumbers! sea1 2 seafloor/move-south!)))
    (cons sea2 (+ count1 count2))))

(assert (= 5 (cdr (seafloor/step *test-seafloor*))))
(assert (= 7 (cdr (seafloor/step (car (seafloor/step *test-seafloor*))))))
;;(display-matrix (car (seafloor/step (matrix-copy *test-seafloor*))))

(define (step-until-stop sea)
  (let loop ((sea sea) (steps 1))
    (let ((res (seafloor/step sea)))
      (cond ((zero? (cdr res)) steps)
            (else
             (if (zero? (modulo steps 50))
                 (begin (display steps) (display ",")))
             (loop (car res) (1+ steps)))))))
(assert (= 58 (step-until-stop *test-seafloor-2*)))

;; part 1
(assert (= 560 (chain "day_25_input.txt"
                      load-file
                      read-seafloor
                      step-until-stop)))
