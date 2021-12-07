;; https://adventofcode.com/2021/day/7

(load "utils.scm")

(define (median ls)
  (let* ((slist (sort ls <))
         (len (length slist)))
    (list-ref slist (quotient len 2))))
(assert (= 2 (median '(16 1 2 0 4 2 7 1 2 14))))

(define (fuel-consumption positions pivot cost)
  (apply + (fold (lambda (pos fuels) (cons (cost (abs (- pos pivot))) fuels))
                 '()
                 positions)))
(assert (= 37 (fuel-consumption '(16 1 2 0 4 2 7 1 2 14) 2 abs)))

;; part 1
(let ((positions (map string->number (string-split (car (load-file "day_7_input.txt")) ", "))))
  (assert (= 357353 (fuel-consumption positions (median positions) abs))))


(define (triangular n)
  (quotient (* n (1+ n)) 2))

(assert (= 168 (fuel-consumption '(16 1 2 0 4 2 7 1 2 14) 5 triangular)))
(assert (= 206 (fuel-consumption '(16 1 2 0 4 2 7 1 2 14) 2 triangular)))

(define (optimize-consumption positions start end)
  (let ((fuel (lambda (n) (fuel-consumption positions n triangular))))
    (cond
     ((> (fuel start) (fuel end)) (optimize-consumption  positions (1+ start) end))
     ((< (fuel start) (fuel end)) (optimize-consumption  positions start (-1+ end)))
     (else
      (cons start (fuel start))))))
(assert (equal? '(5 . 168) (optimize-consumption '(16 1 2 0 4 2 7 1 2 14) 0 16)))

;; part 2
(let ((positions (map string->number (string-split (car (load-file "day_7_input.txt")) ", "))))
  (assert (equal? '(489 . 104822130) (optimize-consumption positions (apply min positions) (apply max positions)))))
