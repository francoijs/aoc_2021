;; https://adventofcode.com/2021/day/17

(load "utils.scm")

(define-structure (area (type list)) x-min x-max y-min y-max)

(define (read-area lines)
  (let* ((tokens (string-split (car lines) "=.,"))
         (x-min (string->number (list-ref tokens 1)))
         (x-max (string->number (list-ref tokens 2)))
         (y-min (string->number (list-ref tokens 4)))
         (y-max (string->number (list-ref tokens 5))))
    (make-area x-min x-max y-min y-max)))
(define *test-area* (read-area '("target area: x=20..30, y=-10..-5")))

(define-structure probe-state x y vx vy)

(define (probe/inside? st area)
  (and (<= (area-x-min area) (probe-state-x st) (area-x-max area))
       (<= (area-y-min area) (probe-state-y st) (area-y-max area))))
(assert (probe/inside? (make-probe-state 4 5 0 0)
                       (make-area 3 4 5 6)))
(assert (not (probe/inside? (make-probe-state 5 5 0 0)
                            (make-area 3 4 5 6))))

(define (probe/above? st area)
  (< (area-y-max area) (probe-state-y st)))
(define (probe/below? st area)
  (> (area-y-min area) (probe-state-y st)))
(define (probe/too-long? st area)
  (< (area-x-max area) (probe-state-x st)))
(define (probe/too-short? st area)
  (> (area-x-min area) (probe-state-x st)))

(define (probe/step st)
  (let ((x (+ (probe-state-x st) (probe-state-vx st)))
        (y (+ (probe-state-y st) (probe-state-vy st)))
        (vx (max 0 (-1+ (probe-state-vx st))))
        (vy (-1+ (probe-state-vy st))))
    (make-probe-state x y vx vy)))

;; simulate trajectory with given initial velocity
;; and return the highest altitude (y) reached,
;; or #f if target area was not hit
(define (probe/send vx vy area)
  (call/cc
   (lambda (return)
     (let iter ((st (make-probe-state 0 0 vx vy)) (max-y 0))
       (cond ((probe/inside? st area) (return max-y))
             ((probe/below? st area) (return #f))
             (else
              (iter (probe/step st)
                    (max max-y (probe-state-y st)))))))))
(assert (probe/send 7 2 *test-area*))
(assert (probe/send 6 3 *test-area*))
(assert (probe/send 9 0 *test-area*))
(assert (not (probe/send 17 -4 *test-area*)))
(assert (= 45 (probe/send 6 9 *test-area*)))

;; try all trajectories for vx,vy in [0, x-max],[y-max, - y-max]
(define (find-highest-trajectory area)
  (assert (negative? (area-y-min area)))
  (let loop ((vx (area-x-max area))
             (vy (* -1 (area-y-min area)))
             (max-alt 0)
             (count 0))
    (cond ((< vy (area-y-min area)) (cons max-alt count))
          ((zero? vx) (loop (area-x-max area) (-1+ vy) max-alt count))
          (else
           (let ((alt (probe/send vx vy area)))
             (loop (-1+ vx) vy
                   (max max-alt (or alt 0))
                   (+ count (if alt 1 0))))))))
(assert (= 45 (car (find-highest-trajectory *test-area*))))

;; part 1
(assert (= 15400 (car (find-highest-trajectory
                       (read-area (load-file "day_17_input.txt"))))))

;; part 2
(assert (= 112 (cdr (find-highest-trajectory *test-area*))))
(assert (= 5844 (cdr (find-highest-trajectory
                      (read-area (load-file "day_17_input.txt"))))))
