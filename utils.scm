;; load file and return list of strings (1 per line)
(define (load-file fname)
  (with-input-from-file fname
    (lambda ()
      (let loop()
        (let ((line (read-string (char-set #\newline))))
          (if (eof-object? line)
              '()
              (begin
                (read-char)
                (cons line (loop)))))))))

(define (curry1 fun arg1)
  (lambda (arg2) (fun arg1 arg2)))
(define curry curry1)
(define (curry2 fun arg2)
  (lambda (arg1) (fun arg1 arg2)))
