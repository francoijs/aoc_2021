;; https://adventofcode.com/2021/day/3

(load "utils.scm")

; e.g: "1001" -> #(1 0 0 1)
(define (diagnostic->vector line)
  (list->vector
   (map char->digit
        (string->list line))))

(assert (equal? '#(1 0 0 1) (diagnostic->vector "1001")))

(define (most-common-bit ls index)
  (<= (/ (length ls) 2)
      (apply + (map (right-curry vector-ref index) ls))))
(define least-common-bit (lambda (ls index) (not (most-common-bit ls index))))

(assert (least-common-bit '(#(0 0 1 0 0)
                            #(1 1 1 1 0)
                            #(1 0 1 0 0)
                            #(1 0 1 1 0)) 4))
(assert (most-common-bit '(#(0 0 1 0 0)
                           #(1 1 1 1 0)
                           #(1 0 1 0 0)
                           #(1 0 1 1 0)) 3))
(assert (most-common-bit '(#(0 0 1 0 0)
                           #(1 1 1 1 0)
                           #(1 0 1 0 0)
                           #(1 0 1 1 0)) 2))
(assert (least-common-bit '(#(0 0 1 0 0)
                            #(1 1 1 1 0)
                            #(1 0 1 0 0)
                            #(1 0 1 1 0)) 1))
(assert (most-common-bit '(#(0 0 1 0 0)
                           #(1 1 1 1 0)
                           #(1 0 1 0 0)
                           #(1 0 1 1 0)) 0))
  

;; part 1

(define (diagnostics->power-rates ls)
  (let* ((vectors (map diagnostic->vector ls))
         (count (length vectors))
         (bits (vector-length (car vectors)))
         (gamma-rate (make-bit-string bits #f)))
    (do ((index 0 (1+ index)))
        ((>= index bits))
      (apply
       (if (most-common-bit vectors index)
           bit-string-set! bit-string-clear!)
       (list gamma-rate (- bits index 1))))
;;    (warn "diags:" count "bits:" bits "gamma=" gamma-rate)
    (cons (bit-string->unsigned-integer gamma-rate)
          (bit-string->unsigned-integer (bit-string-not gamma-rate)))))

(assert (equal? '(22 . 9)
                (diagnostics->power-rates '("00100"
                                            "11110"
                                            "10110"
                                            "10111"
                                            "10101"
                                            "01111"
                                            "00111"
                                            "11100"
                                            "10000"
                                            "11001"
                                            "00010"
                                            "01010"))))

(assert (= 693486
           (let ((rates (diagnostics->power-rates (load-file "day_3_input.txt"))))
             (* (car rates) (cdr rates)))))


;; part 2

(define (vector->rating vec)
  (fold (lambda (idx sum) (+ sum (* (vector-ref vec idx)
                                    (expt 2 (- (vector-length vec) idx 1)))))
        0 (iota (vector-length vec))))

(assert (= 9 (vector->rating '#(1 0 0 1))))

(define (diagnostics->life-ratings ls)
  (let* ((vectors (map diagnostic->vector ls))
         (bits (vector-length (car vectors))))
    (define (filter-out-vectors criterion)
      (let loop((vectors vectors)
                (index 0))
        (cond
         ((null? (cdr vectors)) (car vectors))
         ((>= index bits) (error "no number left!"))
         (else
          (let ((bit (if (criterion vectors index) 1 0)))
            (loop (list-transform-positive
                      vectors
                    (lambda (v) (= bit (vector-ref v index))))
                  (1+ index)))))))
    (cons (vector->rating (filter-out-vectors most-common-bit))
          (vector->rating (filter-out-vectors least-common-bit)))))

(assert (equal? '(23 . 10) (diagnostics->life-ratings '("00100"
                                                        "11110"
                                                        "10110"
                                                        "10111"
                                                        "10101"
                                                        "01111"
                                                        "00111"
                                                        "11100"
                                                        "10000"
                                                        "11001"
                                                        "00010"
                                                        "01010"))))

(assert (= 3379326 (let ((rates (diagnostics->life-ratings (load-file "day_3_input.txt"))))
                     (* (car rates) (cdr rates)))))
