;; https://adventofcode.com/2021/day/6

(define (make-populations ages)
  (fold (lambda (pop vec) (begin
                            (vector-inc! vec pop)
                            vec))
        (make-vector 9 0)
        (map string->number (string-split ages ", "))))
(assert (equal? '#(0 1 1 2 1 0 0 0 0) (make-populations "3,4,3,1,2"))))

(define (populations-step! pop)
(let ((pop0 (vector-ref pop 0)))
  (for-each (lambda (age) (vector-set! pop (-1+ age) (vector-ref pop age)))
            (iota 8 1))
  (vector-set! pop 8 pop0)
  (vector-add! pop 6 pop0)
  pop))
(assert (equal? (make-populations "0,1,0,5,6,0,1,2,2,3,7,8")
                (populations-step!
                 (populations-step!
                  (populations-step!
                   (populations-step!
                    (populations-step!
                     (populations-step!
                      (populations-step!
                       (populations-step!
                        (populations-step!
                         (populations-step!
                          (make-populations "3,4,3,1,2")))))))))))))

(define (iterate-populations pop days)
(apply + (vector->list (fold (lambda (_ pop) (populations-step! pop))
                             pop
                             (iota days)))))
(assert (= 26 (iterate-populations (make-populations "3,4,3,1,2") 18)))
(assert (= 5934 (iterate-populations (make-populations "3,4,3,1,2") 80)))


;; part 1
(assert (= 388739 (iterate-populations (make-populations (car (load-file "day_6_input.txt"))) 80)))


;; part 2
(assert (= 26984457539 (iterate-populations (make-populations "3,4,3,1,2") 256)))
(assert (= 1741362314973 (iterate-populations (make-populations (car (load-file "day_6_input.txt"))) 256)))
