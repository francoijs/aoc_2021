;; https://adventofcode.com/2021/day/5

(load "utils.scm")

(define (read-vents lines)
  (define (string->vent str)
    (let ((vent (map string->number (string-split str ", ->"))))
      (cons (cons (first vent) (second vent))
            (cons (third vent) (fourth vent)))))
  (map string->vent lines))

(assert (equal? '(((0 . 9) 5 . 9) ((8 . 0) 0 . 8) ((9 . 4) 3 . 4))
                (read-vents '("0,9 -> 5,9"
                              "8,0 -> 0,8"
                              "9,4 -> 3,4"))))

(define-generator (make-vent-points-iterator vent)
  (let* ((start (car vent))
         (end (cdr vent))
         (ampl-x (- (car end) (car start)))
         (ampl-y (- (cdr end) (cdr start)))
         (dir-x (if (zero? ampl-x) 0 (/ ampl-x (abs ampl-x))))
         (dir-y (if (zero? ampl-y) 0 (/ ampl-y (abs ampl-y))))
         (stop #f))
    (let loop((dx 0) (dy 0))
      (cond (stop (yield #f))
            ((and (= dx ampl-x) (= dy ampl-y))
             (begin (set! stop #t)
                    (yield end)))
            (else
             (yield (cons (+ (car start) dx) (+ (cdr start) dy)))))
      (loop  (+ dx dir-x)
             (+ dy dir-y)))))

(let ((it (make-vent-points-iterator '((0 . 9) 3 . 9))))
  (assert (equal? '(0 . 9) (it)))
  (assert (equal? '(1 . 9) (it)))
  (assert (equal? '(2 . 9) (it)))
  (assert (equal? '(3 . 9) (it)))
  (assert (equal? #f (it))))

(define (build-vents-diagram vents)
  (let* ((max-x (apply max (append (map caar vents) (map cadr vents))))
         (max-y (apply max (append (map cdar vents) (map cddr vents))))
         (diagram (make-matrix (1+ max-y) (1+ max-x))))
    (let loop((vents vents))
      (if (null? vents)
          diagram
          (let ((it (make-vent-points-iterator (car vents))))
            (do ((point (it) (it)))
                ((not point) #f)
              ;;            (warn point)
              (vector-inc! (vector-ref diagram (cdr point)) (car point)))
            (loop (cdr vents)))))))

(define (straight-vent? vent)
  (or (= (caar vent) (cadr vent))
      (= (cdar vent) (cddr vent))))
(assert (straight-vent? '((0 . 9) 5 . 9)))

(define test-vents '("0,9 -> 5,9"
                     "8,0 -> 0,8"
                     "9,4 -> 3,4"
                     "2,2 -> 2,1"
                     "7,0 -> 7,4"
                     "6,4 -> 2,0"
                     "0,9 -> 2,9"
                     "3,4 -> 1,4"
                     "0,0 -> 8,8"
                     "5,5 -> 8,2"))
(assert (= 5 (matrix-count (build-vents-diagram (filter straight-vent? (read-vents test-vents)))
                           (lambda (n) (> n 1)))))

;; part 1
(matrix-count (build-vents-diagram (filter straight-vent? (read-vents (load-file "day_5_input.txt"))))
              (lambda (n) (> n 1)))))


(assert (= 12 (matrix-count (build-vents-diagram (read-vents test-vents))
                            (lambda (n) (> n 1)))))

;; part 2
(assert (= 17604 (matrix-count (build-vents-diagram (read-vents (load-file "day_5_input.txt")))
                               (lambda (n) (> n 1)))))
