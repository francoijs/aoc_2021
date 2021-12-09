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

; generators
; https://gist.github.com/zeeshanlakhani/1254439
(define-syntax define-generator
  (syntax-rules ()
    ((_ (name arg1 ...) body1 ...)
     (define (name arg1 ...)
       (letrec ((main-logic
                 (lambda (suspend)
                   ;; we're just turning it into a function that
                   ;; can be called within the expanded code.
                   (let ((yield
                          (lambda v
                            (begin
                              (call-with-current-continuation
                               (lambda (new-bail)
                                 (set! main-logic (lambda (cont)
                                                    (set! suspend cont)
                                                    (apply new-bail v)))
                                 (apply suspend v)))))))
                     (let name ((arg1 arg1) ...)
                       body1 ...)))))
         (lambda ()
           (call-with-current-continuation
            (lambda (exit-function)
              (main-logic exit-function)))))))))

(define (iterator->list it)
  (let loop((next (it)) (res '()))
    (if (not next) (reverse res)
        (loop (it) (cons next res)))))

(define (make-matrix rows cols)
  (let loop((mat (make-vector rows)) (line (-1+ rows)))
    (if (= -1 line)
        mat
        (begin
          (vector-set! mat line (make-vector cols 0))
          (loop mat (-1+ line))))))

;; return the number of elements which match predicate
(define (matrix-count mat pred)
  (let loop((rows (vector->list mat)) (count 0))
    (if (null? rows)
        count
        (loop (cdr rows)
              (+ count (fold (lambda (n sum) (+ sum (if (pred n) 1 0)))
                             0 (vector->list (car rows))))))))
(assert (= 9 (matrix-count (make-matrix 3 3) zero?)))
(assert (= 0 (matrix-count (make-matrix 3 3) positive?)))

(define (matrix-height mat) (vector-length mat))
(define (matrix-width mat) (vector-length (vector-ref mat 0)))

; return element at row,col; def or error if out-of-bounds
(define (matrix-ref mat row col #!optional def)
  (cond ((and (< col (matrix-width mat))
              (< row (matrix-height mat)))
         (vector-ref (vector-ref mat row) col))
        ((default-object? def) (error "matrix-ref: invalid pos" row col))
        (else def)))

(define (matrix-set! mat row col cell)
  (assert (< row (matrix-height mat)))
  (assert (< col (matrix-width mat)))
  (vector-set! (vector-ref mat row) col cell))
