;;
;; STRINGS
;;
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

;;
;; LISTS
;;
(define (list-flatten ls)
  (let loop((ls ls))
    (if (null? ls) '()
        (append
         (if (list? (car ls))
             (list-flatten (car ls))
             (list (car ls)))
         (loop (cdr ls))))))
(assert (equal? '(1 2 3 4) (list-flatten '(1 (2 (3)) (4)))))

;; split a list of strings, separated by delim-str, into a list of lists
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

(define set-first! set-car!)
(define set-second! (lambda (l x) (set-cdr! l (cons x (list-tail l 2)))))
(define set-third! (lambda (l x) (set-cdr! (cdr l) (cons x (list-tail l 3)))))

;; remove duplicates in a sorted list
(define (uniq lst)
  (cond
   ((null? lst) '())
   ((null? (cdr lst)) lst)
   ((equal? (car lst) (cadr lst)) (uniq (cdr lst)))
   (else (cons (car lst) (uniq (cdr lst))))))
(assert (equal? '(1 2 3) (uniq '(1 2 2 3 3 3))))

(define (integer->list digits n)
  (let ((res (let loop((n n))
               (if (zero? n) '()
                   (cons (modulo n 10) (loop (quotient n 10)))))))
    (append (make-list (- digits (length res)) 0) (reverse res))))
(assert (equal? '(0 1 2 3 4) (integer->list 5 1234)))

(define (list->integer ls)
  (fold (lambda (d s) (+ (* 10 s) d))
        0 ls))
(assert (equal? 1234 (list->integer '(0 1 2 3 4))))

;;
;; VECTORS
;;
(define (vector-set vec k obj)
  (let ((vec (vector-copy vec)))
    (vector-set! vec k obj)
    vec))

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

;;
;; MATRICES
;;
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

(define (read-matrix lines . conv)
  (let ((conv (if (null? conv)
                  (lambda (c) (- (char->integer c)
                                 (char->integer #\0)))
                 (car conv))))
    (let loop((lines lines) (row 0) (matrix (make-vector 0 0)))
      (if (null? lines) matrix
          (loop (cdr lines) (1+ row)
                (vector-grow-set! matrix row
                                  (list->vector (map conv
                                                 (string->list (car lines))))))))))

(define (build-matrix rows cols proc)
  (let loop((mat (make-vector rows)) (row (-1+ rows)))
    (if (< row 0) mat
        (begin
          (vector-set! mat row (make-initialized-vector
                                cols (lambda (col) (proc row col))))
          (loop mat (-1+ row))))))
(define make-initialized-matrix build-matrix)

(define (matrix-map proc mat)
  (vector-map (lambda(v) (vector-map proc v))
              mat))

(define (matrix-copy mat)
  (vector-map vector-copy mat))

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
(define (matrix-dimensions mat) (cons (matrix-height mat) (matrix-width mat)))

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

(define (matrix-fold proc accu0 mat)
  (let ((accu accu0))
    (matrix-for-each (lambda (r c obj)
                       (set! accu (proc obj accu)))
                     mat)
    accu))
(assert (= 4 (matrix-fold + 0 (make-initialized-matrix 2 2 +))))

(define (submatrix mat row col subh subw)
  (assert (<= (+ row subh) (matrix-height mat)))
  (assert (<= (+ col subw) (matrix-width  mat)))
  (vector-map (lambda(v)
                (subvector v col (+ col subw)))
              (subvector mat row (+ row subh))))

(define (submatrix-copy! mat0 r0 c0 h w mat1 r1 c1)
  (assert (<= 0 (+ r0 h) (matrix-height mat0)))
  (assert (<= 0 (+ c0 w) (matrix-width  mat0)))
  (assert (<= 0 (+ r1 h) (matrix-height mat1)))
  (assert (<= 0 (+ c1 w) (matrix-width  mat1)))
  (matrix-for-each (lambda (r c val)
                     (matrix-set! mat1 (+ r1 (- r r0))
                                  (+ c1 (- c c0)) val))
                   (submatrix mat0 r0 c0 h w)))

;; flip a matrix horizontally (dir=0), vertically (dir=1),
;; or diagonally (dir=2)
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

(define (matrix-equal? m1 m2)
  (call/cc
   (lambda (return)
     (matrix-for-each (lambda (y x obj)
                        (if (not (equal? obj
                                         (matrix-ref m2 y x)))
                            (return #f)))
                      m1)
     (return #t))))

(define (display-matrix mat . proc)
  (let ((proc (if (null? proc)
                  (lambda (n) (digit->char n))
                  (car proc))))
    (newline)
    (for-each (lambda (row)
                (begin
                  (vector-map (compose display proc)
                              row)
                  (newline)))                    
              (vector->list mat))))

(define (display-bit-matrix mat)
  (newline)
  (for-each (lambda (row) (begin
                            (vector-map (compose display (lambda (b) (if b "#" ".")))
                                        row)
                            (newline)))                    
            (vector->list mat)))

;;
;; HASH-TABLES
;;
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

;;
;; BIT-STRINGS
;;
(define (bit-string-map proc bs)
  (let ((len (bit-string-length bs)))
    (fold (lambda (idx ls) (cons (proc (bit-string-ref bs idx)) ls))
          '()
          (iota len))))
(assert (equal? '(#t #f #t #f) (bit-string-map identity (unsigned-integer->bit-string 4 10))))

(define (bit-string-mod-range bs start end proc)
  (let ((bs1 (unsigned-integer->bit-string
              (bit-string-length bs)
              (- (expt 2 end) (expt 2 start)))))
    (proc bs bs1)))

(define bit-string-set-range! (right-curry bit-string-mod-range bit-string-or!))
(define bit-string-set-range (right-curry bit-string-mod-range bit-string-or))
(assert (equal? '#*10111110 (bit-string-set-range '#*10101010 1 5)))
(define bit-string-clear-range! (right-curry bit-string-mod-range bit-string-andc!))
(define bit-string-clear-range (right-curry bit-string-mod-range bit-string-andc))
(assert (equal? '#*10100000 (bit-string-clear-range '#*10101010 1 5)))

(define u2bs unsigned-integer->bit-string)
(define bs2u bit-string->unsigned-integer)

(define (bit-string-count-ones bs0)
  (let loop ((bs bs0) (count 0))
    (if (bit-string-zero? bs)
        count
        (loop (bit-string-and bs (u2bs (bit-string-length bs0)
                                       (-1+ (bs2u bs))))
              (1+ count)))))
(assert (= 2 (bit-string-count-ones '#*010100)))
(assert (= 4 (bit-string-count-ones '#*011101)))

;;
;; FP
;;
(define identity (lambda (x) x))

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

(define (curry func . curry-args)
  (lambda args
    (apply func (append curry-args args))))

(define (right-curry func . curry-args)
  (lambda args
    (apply func (append args curry-args))))

;;
;; OTHER
;;
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

(define (combinations ls n)
  (cond
   ((zero? n) '(()))
   ((>= n (length ls)) (list ls))
   (else (append
          (combinations (cdr ls) n)
          (map (lambda (comb)
                 (cons (car ls) comb))
               (combinations (cdr ls) (-1+ n)))))))
(assert (equal? '((2 3) (1 3) (1 2)) (combinations '(1 2 3) 2)))

(define (accessor col)
  (let ((proc (cond ((vector? col) vector-ref)
                    ((list? col) list-ref)
                    ((bit-string? col) bit-string-ref)
                    (else
                     (error "unknown collection type for" col)))))
    (lambda (i) (proc col i))))
