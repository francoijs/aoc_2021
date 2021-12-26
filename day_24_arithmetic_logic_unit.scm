;; https://adventofcode.com/2021/day/24

(load "utils.scm")

(define (read-instruction str)
  (let* ((tok (accessor (string-split str " ")))
         (sym (lambda (i)
                (if (string-find-next-char-in-set (tok i) char-set:alphabetic)
                    (string->symbol (tok i))
                    (string->number (tok i))))))
    (cond 
      ((string=? (tok 0) "inp") (list 'set! (sym 1) 'input))
      ((string=? (tok 0) "add") (list 'set! (sym 1) (list '+ (sym 1) (sym 2))))
      ((string=? (tok 0) "mul") (list 'set! (sym 1) (list '* (sym 1) (sym 2))))
      ((string=? (tok 0) "div") (list 'set! (sym 1) (list 'quotient (sym 1) (sym 2))))
      ((string=? (tok 0) "mod") (list 'set! (sym 1) (list 'modulo (sym 1) (sym 2))))
      ((string=? (tok 0) "eql") (list 'set! (sym 1) (list 'if (list '= (sym 1) (sym 2)) 1 0)))
      (else
       (error "unknown instruction" str)))))
(assert (equal? '(set! a input) (read-instruction "inp a")))
(assert (equal? '(set! a (+ a b)) (read-instruction "add a b")))

(define (list-replace ls obj inputs)
  (let ((inputs inputs))
    (let loop ((ls ls))
      (cond ((null? ls) '())
            ((list? (car ls)) (let ((cls (loop (car ls)))) ; force eval 1st
                                (cons cls (loop (cdr ls)))))
            ((equal? obj (car ls)) (let ((input (car inputs)))
                                     (set! inputs (cdr inputs))
                                     (cons input (loop (cdr ls)))))
            (else
             (cons (car ls) (loop (cdr ls))))))))
(assert (equal? '(1 2 3 4 5) (list-replace '(1 a 3 a 5) 'a '(2 4))))
(assert (equal? '((1 2) 3 (4 5)) (list-replace '((1 a) 3 (a 5)) 'a '(2 4))))

(define (read-alu-program lines)
  (let ((ins (map read-instruction lines) ))
    (lambda (input)
      (let ((ins (list-replace ins 'input input)))
        (eval
         (append '(let ((w 0) (x 0) (y 0) (z 0)))
                 ins
                 '((values w x y z)))
         user-initial-environment)))))

(receive (w x y z) ((read-alu-program '("inp x"
                                        "mul x -1")) '(42))
  (assert (= x -42)))
(receive (w x y z) ((read-alu-program '("inp z"
                                        "inp x"
                                        "mul z 3"
                                        "eql z x")) '(42 126))
  (assert (= z 1)))
(receive (w x y z) ((read-alu-program '("inp w"
                                        "add z w"
                                        "mod z 2"
                                        "div w 2"
                                        "add y w"
                                        "mod y 2"
                                        "div w 2"
                                        "add x w"
                                        "mod x 2"
                                        "div w 2"
                                        "mod w 2")) '(10))
  (assert (and (= w 1) (= x 0) (= y 1) (= z 0))))

(define-generator (make-sn-generator rank)
  (let loop ((sn (make-vector 14 9)) (carry 0))
    (unless (positive? carry)
      (yield (reverse (vector->list sn))))
    (cond ((= rank carry) (yield #f))
          ((= 1 (vector-ref sn carry))
           (begin
             (vector-set! sn carry 9)
             (loop sn (1+ carry))))
          (else
           (vector-dec! sn carry)
           (loop sn 0)))))
(let ((g (make-sn-generator 2)))
  (assert (= 99999999999999 (list->integer (g))))
  (assert (= 99999999999998 (list->integer (g))))
  (assert (= 99999999999997 (list->integer (g))))
  (assert (= 99999999999996 (list->integer (g))))
  (assert (= 99999999999995 (list->integer (g))))
  (assert (= 99999999999994 (list->integer (g))))
  (assert (= 99999999999993 (list->integer (g))))
  (assert (= 99999999999992 (list->integer (g))))
  (assert (= 99999999999991 (list->integer (g))))
  (assert (= 99999999999989 (list->integer (g))))
)

;; brute force search 
(define (search-valid-sn nmax)
  (call/cc
   (lambda (return)
     (let ((prog (read-alu-program (load-file "day_24_input.txt")))
           (gen (make-sn-generator nmax)))
       (let loop ((sn (gen)) (count 0))
         (if (not sn) (return #f))
         (if (zero? (modulo count 100))
             (begin (display (list->integer sn)) (display ",")))
         (receive (w x y z) (prog sn)
           (if (zero? z) (return sn)
               (loop (gen) (1+ count)))))))))
(search-valid-sn 10)

;; from reading https://www.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/, it seems a better strategy to analyze what the assembly does, and find a matching S/N by hand.
;; TBC
