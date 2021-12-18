;; https://adventofcode.com/2021/day/15

(load "utils.scm")

(define (read-risk-map lines)
  (read-matrix lines))
(define *test-map* (read-risk-map '("1163751742"
                                    "1381373672"
                                    "2136511328"
                                    "3694931569"
                                    "7463417111"
                                    "1319128137"
                                    "1359912421"
                                    "3125421639"
                                    "1293138521"
                                    "2311944581")))

(define (find-min-risk mat)
  (let* ((w (matrix-width mat))
         (h (matrix-height mat))
         (sums (make-matrix h w)))
    (let loop ((x (-1+ w))
               (y (-1+ h)))
      (cond ((= -1 x) (- (matrix-ref sums 0 0) (matrix-ref mat 0 0)))
            ((= -1 y) (loop (-1+ x) (-1+ h)))
            (else
             (begin
               (matrix-set! sums y x
                            (+ (matrix-ref mat y x)
                               (cond ((and (= x (-1+ w)) (= y (-1+ h))) 0)
                                     ((= x (-1+ w)) (matrix-ref sums (1+ y) x))
                                     ((= y (-1+ h)) (matrix-ref sums y (1+ x)))
                                     (else (min (matrix-ref sums (1+ y) x)
                                                (matrix-ref sums y (1+ x)))))))
               (loop x (-1+ y))))))))
(assert (= 40 (find-min-risk *test-map*)))

;; part 1 - FIXME shall be 537 !
(chain (load-file "day_15_input.txt")
       read-risk-map
       find-min-risk)


(define (increment-risk lvl inc)
  (let ((new (+ lvl inc)))
    (if (> new 9) (- new 9) new)))
(assert (= 9 (increment-risk 8 1)))
(assert (= 1 (increment-risk 8 2)))
(assert (= 2 (increment-risk 8 3)))
(assert (= 1 (increment-risk 9 1)))
(assert (= 2 (increment-risk 1 1)))

(define (large-risk-map mat)
  (let ((new-mat (make-matrix (* 5 (matrix-height mat))
                              (* 5 (matrix-width mat)))))
    (let loop ((i 0) (j 0) (risk-inc 0))
      (cond ((= j 5) new-mat)
            ((= i 5) (loop 0 (1+ j) (- risk-inc 4)))
            (else (begin
                    (matrix-for-each (lambda (row col val)
                                       (matrix-set! new-mat
                                                    (+ row (* i (matrix-height mat)))
                                                    (+ col (* j (matrix-width mat)))
                                                    (increment-risk val risk-inc)))
                                     mat)
                    (loop (1+ i) j (1+ risk-inc))))))))

(assert (= 315 (chain *test-map*
                      large-risk-map
                      find-min-risk)))


;; part 2 - FIXME
(chain (load-file "day_15_input.txt")
       read-risk-map
       large-risk-map
       find-min-risk)



;; SLOW METHOD USING A*

;;(load "astar.scm")
;;
;; (define (edge-risk e)
;;   (assert (map-edge? e))
;;   (let* ((n (map-edge:dst e)))
;;     (matrix-ref (map-node:M n) (map-node:x n) (map-node:y n))))

;; (define (find-min-risk mat)
;;   (apply + (map edge-risk
;;                 (A* mat
;;                     (make-map-node mat 0 0)
;;                     (make-node-cost (-1+ (matrix-width mat)) (-1+ (matrix-height mat)))
;;                     edge-risk))))
;; (assert (= 40 (find-min-risk *test-map*)))

;; ;; part 1
;; (assert (= 537 (find-min-risk (read-risk-map (load-file "day_15_input.txt")))))

;; ;; part 2
;; (chain (load-file "day_15_input.txt")
;;        read-risk-map
;;        large-risk-map
;;        find-min-risk)
