;; https://adventofcode.com/2021/day/1

(define (count-increases ls)
  (if (null? ls)
      0
      (let loop ((ls (cdr ls)) (prev-depth (car ls)))
        (if (null? ls)
            0
            (+ (if (> (car ls) prev-depth) 1 0)
               (loop (cdr ls) (car ls)))))))

(assert (= 0 (count-increases '())))
(assert (= 0 (count-increases '(1))))
(assert (= 7 (count-increases '(199 200 208 210 200 207 240 269 260 263))))


(define (load-depths fname)
  (define (line->depth line)
    (with-input-from-string line
      (lambda ()
        (let ((word (read-string (char-set #\newline))))
          (begin
            (read-char)
            (string->number word))))))
  (with-input-from-file fname
    (lambda ()
      (let loop((line (read-line)))
;;        (warn line)
        (if (eof-object? line)
            '()
            (cons (line->depth line)
                  (loop (read-line))))))))

(count-increases (load-depths "day_1_input.txt"))
;; 1583


(define (sliding-sums ls window-size)
  (if (< (length ls) window-size)
      '()
      (cons (apply + (list-head ls window-size))
            (sliding-sums (cdr ls) window-size))))

(assert (equal? (sliding-sums
                 '(199 200 208 210 200 207 240 269 260 263) 3)
                '(607 618 618 617 647 716 769 792)))

(count-increases (sliding-sums (load-depths "day_1_input.txt") 3))
;; 1627
