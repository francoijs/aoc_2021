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

;; split str in list of tokens delimited by the chars of delims
(define (string-split str delims)
  (let ((delims (string->char-set delims)))
    (with-input-from-string str
      (lambda ()
        (let loop((word (read-string delims)))
          (if (eof-object? word) '()
              (begin
                (read-char)
                (append 
                 (if (zero? (string-length word))
                     '() (list word))
                 (loop (read-string delims))))))))))
(assert (equal? '("12" "3" "456")
                (string-split ",12 3,456 " " ,")))

(define (list-flatten ls)
  (let loop((ls ls))
    (if (null? ls) '()
        (append
         (if (list? (car ls))
             (list-flatten (car ls))
             (list (car ls)))
         (loop (cdr ls))))))
(assert (equal? '(1 2 3 4) (list-flatten '(1 (2 (3)) (4)))))

;; split a list of strings, separated by delim-str, in a list of lists
(define (list-split ls delim-str)
  (if (null? ls) '()
      (let loop((ls ls) (group '()))
        (cond
         ((null? ls) (list (reverse group)))
         ((equal? (car ls) delim-str)
          (append (if (null? group) '() (list (reverse group)))
                  (list-split (cdr ls) delim-str)))
         (else
          (loop (cdr ls) (cons (car ls) group)))))))
(assert (equal? '() (list-split '() "az")))
(assert (equal? '() (list-split '("az") "az")))
(assert (equal? '(("1") ("2" "3")) (list-split '("1" "az" "2" "3") "az")))

;; set element of vector, expanding it if necessary
(define (vector-grow-set! vec idx elt)
  (if (>= idx (vector-length vec))
      (set! vec (vector-grow vec (1+ idx))))
  (vector-set! vec idx elt)
  vec)
(assert (equal? '#(0) (vector-grow-set! '#() 0 0)))
(assert (equal? '#(0 #f 2) (vector-grow-set! '#(0) 2 2)))
(assert (equal? '#(0 1 2) (vector-grow-set! '#(0 0 2) 1 1)))

(define (vector-add! v i obj)
  (vector-set! v i (+ obj (vector-ref v i))))
(define (vector-inc! v i)
  (vector-add! v i 1))
(define (vector-dec! v i)
  (vector-add! v i -1))

;; remove duplicates in a sorted list
(define (uniq lst)
  (cond
   ((null? lst) '())
   ((null? (cdr lst)) lst)
   ((equal? (car lst) (cadr lst)) (uniq (cdr lst)))
   (else (cons (car lst) (uniq (cdr lst))))))
(assert (equal? '(1 2 3) (uniq '(1 2 2 3 3 3))))
