;; https://adventofcode.com/2021/day/17

(load "utils.scm")

(define-record-type :area
  (make-area x-min x-max y-min y-max)
  area?
  (x-min area:x-min)
  (x-max area:x-max)
  (y-min area:y-min)
  (y-max area:y-max))

(define (read-area lines)
  (let* ((tokens (string-split (car lines) "=.,"))
         (x-min (string->number (list-ref tokens 1)))
         (x-max (string->number (list-ref tokens 2)))
         (y-min (string->number (list-ref tokens 4)))
         (y-max (string->number (list-ref tokens 5))))
    (make-area x-min x-max y-min y-max)))
(define *test-area* (read-area '("target area: x=20..30, y=-10..-5")))

(define-record-type :probe-state
  (make-probe-state x y vx vy)
  probe-state?
  (x probe-state:x)
  (y probe-state:y)
  (vx probe-state:vx)
  (vy probe-state:vy))

(define (probe/inside? st area)
  (and (<= (area:x-min area) (probe-state:x st) (area:x-max area))
       (<= (area:y-min area) (probe-state:y st) (area:y-max area))))
(assert (probe/inside? (make-probe-state 4 5 0 0)
                       (make-area 3 4 5 6)))
(assert (not (probe/inside? (make-probe-state 5 5 0 0)
                            (make-area 3 4 5 6))))

(define (probe/above? st area)
  (< (area:y-max area) (probe-state:y st)))
(define (probe/below? st area)
  (> (area:y-min area) (probe-state:y st)))
(define (probe/too-long? st area)
  (< (area:x-max area) (probe-state:x st)))
(define (probe/too-short? st area)
  (> (area:x-min area) (probe-state:x st)))

(define (probe/step st)
  (let ((x (+ (probe-state:x st) (probe-state:vx st)))
        (y (+ (probe-state:y st) (probe-state:vy st)))
        (vx (max 0 (-1+ (probe-state:vx st))))
        (vy (-1+ (probe-state:vy st))))
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
                    (max max-y (probe-state:y st)))))))))
(assert (probe/send 7 2 *test-area*))
(assert (probe/send 6 3 *test-area*))
(assert (probe/send 9 0 *test-area*))
(assert (not (probe/send 17 -4 *test-area*)))
(assert (= 45 (probe/send 6 9 *test-area*)))

;; starting from an initial velocity that do not hit target area,
;; look for any (vx . vy) that does
(define (find-hit-trajectory vx vy area)
  ;  (warn vx vy)
  (let iter ((st (make-probe-state 0 0 vx vy)))
    (cond ((probe/inside? st area) (cons vx vy))
          ((probe/below? st area)
           (find-hit-trajectory (+ vx (if (probe/too-short? st area) 1 -1))
                                vy
                                area))
          (else
           (iter (probe/step st))))))
(assert (equal? '(6 . 5) (find-hit-trajectory 5 5 *test-area*)))

;; perform steepest ascent on probe/send(vy, vy)
(define (find-highest-trajectory vx0 vy0 area)
  
  ;; trajectory = (vx vy alt)
  (define (adjacent-trajectories vx vy)
    (let next-adj ((dx -1) (dy -1) (res '())) ; iterate dx & dy over [-1 0 1]
      (cond ((= dx 2) res)
            ((= dy 2) (next-adj (1+ dx) -1 res))
            ((= 0 dx dy) (next-adj dx (1+ dy) res)) 
            (else
             (let* ((vx (+ vx dx))
                    (vy (+ vy dy))
                    (alt (probe/send vx vy area)))
               (next-adj dx (1+ dy) (if alt
                                        (cons (list vx vy alt) res)
                                        res)))))))
  (define (trajectory-cmp t1 t2)
    (> (third t1) (third t2)))
  
  ;; find initial trajectory that hit the target
  (let* ((vel0 (find-hit-trajectory vx0 vy0 area))
         (vx0 (car vel0))
         (vy0 (cdr vel0)))
    
    (let ascent ((vx vx0) (vy vy0) (visited '()))
      (let ((alt (probe/send vx vy area))
            (adj (sort (adjacent-trajectories vx vy)
                       trajectory-cmp)))
        ;      (warn vx vy alt adj)
        (if (or (null? adj)                    ; no neighbor reaches the target
                (< (third (car adj)) alt)      ; no better neighbor
                (member (cons vx vy) visited)) ; solution was already visited
            (list vx vy alt)
            (ascent (first (car adj)) (second (car adj))
                    (cons (cons vx vy) visited)))))))
(assert (equal? '(7 9 45) (find-highest-trajectory 7 2 *test-area*)))

;; part 1
(let* ((area (read-area (load-file "day_17_input.txt")))
       (vi (find-hit-trajectory 15 15 area)))
  (find-highest-trajectory (car vi) (cdr vi) area))
