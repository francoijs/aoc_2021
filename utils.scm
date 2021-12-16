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

(define (list-index-of ls obj)
  (let loop ((index 0) (ls ls))
    (cond ((null? ls) #f)
          ((equal? obj (car ls)) index)
          (else
           (loop (1+ index) (cdr ls))))))
(assert (eq? #f (list-index-of '(1 2 3) 0)))
(assert (eq? 0 (list-index-of '(1 2 3) 1)))
(assert (eq? 2 (list-index-of '(1 2 3) 3)))

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

(define (vector-reverse v)
  (list->vector (reverse (vector->list v))))

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

(define (make-matrix rows cols #!optional val)
  (let loop((mat (make-vector rows)) (line (-1+ rows)))
    (if (= -1 line)
        mat
        (begin
          (vector-set! mat line (make-vector cols (if (default-object? val) 0 val)))
          (loop mat (-1+ line))))))
(assert (= 1 (vector-ref (vector-ref (make-matrix 3 2 1) 2) 1)))
(assert (= 0 (vector-ref (vector-ref (make-matrix 3 2) 2) 1)))

;; read matrix from a list of strings made of digits ; add lines and columns around with value 'border'
(define (read-matrix-add-border lines border)
  (let* ((matrix
          (let loop((lines lines) (row 1) (matrix (make-vector 1 border)))
            (if (null? lines) (vector-grow-set! matrix row #f)
                (loop (cdr lines) (1+ row)
                      (vector-grow-set! matrix row
                                        (list->vector (append (list border)
                                                              (map (lambda (c) (- (char->integer c)
                                                                                  (char->integer #\0)))
                                                                   (string->list (car lines)))
                                                              (list border))))))))
         (cols (vector-length (vector-ref matrix 1)))
         (rows (matrix-height matrix)))
    (vector-set! matrix 0 (make-vector cols border))
    (vector-set! matrix (-1+ rows) (make-vector cols border))
    matrix))

(define (matrix-map proc mat)
  (vector-map (lambda(v) (vector-map proc v))
              mat))

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
(define (matrix-width mat) (if (zero? (vector-length mat)) 0 (vector-length (vector-ref mat 0))))

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

(define (matrix-grow-set! mat row col cell)
  (let ((row-vector (vector-grow-set!
                     (if (< row (vector-length mat))
                         (vector-ref mat row)
                         (make-vector 0))
                     col cell)))
    (vector-map (lambda (row)
                  (cond ((not row) (make-vector (vector-length row-vector)))
                        ((> (vector-length row) (vector-length row-vector)) row)
                        (else (vector-grow row (vector-length row-vector)))))
                (vector-grow-set! mat row row-vector))))
(assert (= 42 (matrix-ref (matrix-grow-set! (make-matrix 2 2) 0 0 42) 0 0)))
(assert (= 42 (matrix-ref (matrix-grow-set! (make-matrix 0 0) 1 1 42) 1 1)))
(assert (= 42 (matrix-ref (matrix-grow-set! (make-matrix 0 0) 0 0 42) 0 0)))

(define (matrix-inc! mat row col)
  (vector-set! (vector-ref mat row) col (1+ (matrix-ref mat row col))))

(define (matrix-for-each proc mat)
  (let ((w (matrix-width mat))
        (h (matrix-height mat)))
    (let loop ((row 0) (col 0))
      (unless (= h row)
        (if (= w col)
            (loop (1+ row) 0)
            (begin (proc row col (matrix-ref mat row col))
                   (loop row (1+ col))))))))

(define (submatrix mat row col subh subw)
  (assert (<= (+ row subh) (matrix-height mat)))
  (assert (<= (+ col subw) (matrix-width  mat)))
  (vector-map (lambda(v)
                (subvector v col (+ col subw)))
              (subvector mat row (+ row subh))))

; flip a matrix horizontally (dir=0), vertically (dir=1),
; or diagonally (dir=2)
(define (matrix-flip mat #!optional dir)
  (cond
   ((or (default-object? dir) (= dir 0))
    (vector-map vector-reverse mat))
   ((= dir 1) (vector-reverse mat))
   (else
;    (assert (matrix-square? mat))
    (vector-map (lambda(n)
                  (vector-map (lambda(l) (vector-ref l n))
                              mat))
                (list->vector (iota (matrix-width mat)))))))

(define-syntax compose
  (syntax-rules ()
    ((compose . funs)
     (lambda (arg) (fold-right (lambda (fun result)
                            (fun result))
                          arg
                          (list . funs))))))

(define-syntax chain
  (syntax-rules ()
    ((compose arg . funs)
     ((lambda (a) (fold (lambda (fun result) (fun result))
                        a
                        (list . funs))) arg))))

(define (hash-table/add! table key added)
  (let ((val (hash-table/get table key 0)))
    (assert (number? val))
    (hash-table/put! table key (+ val added))
    table))
(define (hash-table/inc! table key)
  (hash-table/add! table key 1))
(define (hash-table/dec! table key)
  (hash-table/add! table key -1))

(define (hash-table/copy table)
  (fold (lambda (pair copy) (begin
                              (hash-table/put! copy (car pair) (cdr pair))
                              copy))
        (make-equal-hash-table)
        (hash-table->alist table)))

(define (string-hash-table/copy table)
  (fold (lambda (pair copy) (begin
                              (hash-table/put! copy (car pair) (cdr pair))
                              copy))
        (make-string-hash-table)
        (hash-table->alist table)))
(let ((table (make-string-hash-table)))
  (hash-table/put! table "toto" 1)
  (hash-table/put! table "titi" 2)
  (assert (= 1 (hash-table/get (string-hash-table/copy table) "toto" #f)))
  (assert (= 2 (hash-table/get (string-hash-table/copy table) "titi" #f))))
