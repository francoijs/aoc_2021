;; https://adventofcode.com/2021/day/2

;; part 1

(define (apply-command-1 command pos)
  (let ((unit (cdr command)))
    (case (car command)
      ((forward) (cons (+ (car pos) unit) (cdr pos)))
      ((up) (cons (car pos) (- (cdr pos) unit)))
      ((down) (cons (car pos) (+ (cdr pos) unit)))
      (else
       (error "unknown command" command)))))

(assert (equal? '(0 . -3) (apply-command-1 '(up . 3) '(0 . 0))))
(assert (equal? '(0 . 3) (apply-command-1 '(down . 3) '(0 . 0))))
(assert (equal? '(3 . 0) (apply-command-1 '(forward . 3) '(0 . 0))))

(define (apply-commands-1 commands pos)
  (fold apply-command-1 '(0 . 0) commands))

(assert (equal? '(15 . 10) (apply-commands-1 '((forward . 5)
                                               (down . 5)
                                               (forward . 8)
                                               (up . 3)
                                               (down . 8)
                                               (forward . 2)) '(0 . 0))))


(define (load-commands fname)
  (define (line->command line)
    (with-input-from-string line
      (lambda ()
        (let ((direction (string->symbol (read-string (char-set #\space)))))
          (begin
            (read-char)
            (let ((units (read-string (char-set #\newline))))
              (read-char)
              (cons direction (string->number units))))))))
  (with-input-from-file fname
    (lambda ()
      (let loop((line (read-line)))
;        (warn line)
        (if (eof-object? line)
            '()
            (cons (line->command line)
                  (loop (read-line))))))))

(let ((pos (apply-commands (load-commands "day_2_input.txt") '(0 . 0))))
  (* (car pos) (cdr pos)))
;; 1804520


;; part 2

(define-record-type :sub-state
  (make-sub-state hpos depth aim)
  sub-state?
  (hpos sub-state:hpos)
  (depth sub-state:depth)
  (aim sub-state:aim))

(define (apply-command command state)
  (let ((unit (cdr command))
        (hpos (sub-state:hpos state))
        (depth (sub-state:depth state))
        (aim (sub-state:aim state)))
    (case (car command)
      ((forward) (make-sub-state (+ hpos unit)
                                 (+ depth (* unit aim))
                                 aim))
      ((up) (make-sub-state hpos depth (- aim unit)))
      ((down) (make-sub-state hpos depth (+ aim unit)))
      (else
       (error "unknown command" command)))))

(assert (equal? -3 (sub-state:aim (apply-command '(up . 3) (make-sub-state 0 0 0)))))
(assert (equal?  3 (sub-state:aim (apply-command '(down . 3) (make-sub-state 0 0 0)))))
(assert (equal?  3 (sub-state:hpos (apply-command '(forward . 3) (make-sub-state 0 0 0)))))
(assert (equal?  3 (sub-state:depth (apply-command '(forward . 3) (make-sub-state 0 0 1)))))
(assert (equal?  1 (sub-state:aim  (apply-command '(forward . 3) (make-sub-state 0 0 1)))))

(let ((new-state (fold apply-command (make-sub-state 0 0 0) (load-commands "day_2_input.txt"))))
  (* (sub-state:hpos new-state)
     (sub-state:depth new-state)))
;; 1971095320
