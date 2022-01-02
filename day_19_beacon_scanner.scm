;; https://adventofcode.com/2021/day/19

(load "utils.scm")

(define-structure (beacon (type vector)) x y z)

(define (beacon->string b)
  (apply (string-joiner 'infix ",")
         (map number->string (vector->list b))))
(define (string->beacon str)
  (let ((tok (map string->number (string-split str ","))))
    (make-beacon (list-ref tok 0)
                 (list-ref tok 1)
                 (if (< (length tok) 3) 0 (list-ref tok 2)))))
(assert (string=? "1,2,3" (beacon->string (string->beacon "1,2,3"))))
(assert (string=? "1,2,0" (beacon->string (string->beacon "1,2"))))

;; read vector of lists of beacons
(define (read-scanners lines)
  (list->vector (map (lambda (group) (map string->beacon (cdr group)))
                     (list-split lines ""))))
(define *test-scanners* (accessor (read-scanners '("--- scanner 0 ---"
                                                   "0,2"
                                                   "4,1"
                                                   "3,3"
                                                   ""
                                                   "--- scanner 1 ---"
                                                   "-1,-1"
                                                   "-5,0"
                                                   "-2,1"))))
(assert (string=? (beacon->string (list-ref (*test-scanners* 1) 1))
                  "-5,0,0"))

;; distance between 2 beacons
(define (beacon/distance b1 b2)
  (sqrt (+ (square (- (beacon-x b1) (beacon-x b2)))
           (square (- (beacon-y b1) (beacon-y b2)))
           (square (- (beacon-z b1) (beacon-z b2))))))
(assert (= 5 (beacon/distance (make-beacon 4 -3 1)
                              (make-beacon 1  1 1))))

;; 1 "distance" = '(distance beacon1 beacon2)
;;(define (distance-equal? d1 d2) (= (car d1) (car d2)))
(define (distance-equal? d1 d2) (< (abs (- (car d1) (car d2))) .001))
;; (define (distance-cmp d1 d2) (and (< (car d1) (car d2))
;;                                   (not (distance-equal? d1 d2))))
(define (distance-cmp d1 d2) (< (car d1) (car d2)))

;; sorted list of distances between the beacons from a scanner
(define (get-distances ls)
  (sort (let loop ((ls ls))
          (if (null? ls) '()
              (append (fold (lambda (b res)
                              (cons (list (beacon/distance b (car ls))
                                          b (car ls))
                                    res))
                            '() (cdr ls))
                      (loop (cdr ls)))))
        distance-cmp))
(assert (= 3 (length (get-distances (*test-scanners* 0)))))

;; extract pairs of beacons with equal distance found in 2 sorted lists
;; -> list of ( (b1.1 . b1.2) . (b2.1 . b2.2) )
(define (equal-distances ls1 ls2)
  (let loop ((ls1 ls1) (ls2 ls2))
    (cond ((or (null? ls1) (null? ls2)) '())
          ((distance-cmp (car ls1) (car ls2)) (loop (cdr ls1) ls2))
          ((distance-cmp (car ls2) (car ls1)) (loop ls1 (cdr ls2)))
          (else (cons (cons (car ls1) (car ls2))
                      (loop (cdr ls1) (cdr ls2)))))))
(assert (= 3 (length (equal-distances (get-distances (*test-scanners* 0))
                                      (get-distances (*test-scanners* 1))))))

(define *test-scanners-2* (chain "day_19_test_input.txt"
                                 load-file
                                 read-scanners))
(define get-scanner (accessor *test-scanners-2*))
;; 12 common beacons = 11*12/2 equal distances
(assert (= 66 (length (equal-distances (get-distances (get-scanner 0))
                                       (get-distances (get-scanner 1))))))
(assert (= 66 (length (equal-distances (get-distances (get-scanner 1))
                                       (get-distances (get-scanner 4))))))

(define (3x3-matrix a b c d e f g h i)
  (let ((mat (make-vector 3)))
    (vector-set! mat 0 (list->vector (list a b c)))
    (vector-set! mat 1 (list->vector (list d e f)))
    (vector-set! mat 2 (list->vector (list g h i)))
    mat))
(assert (equal? '(3 . 3) (matrix-dimensions (3x3-matrix 1 2 3 4 5 6 7 8 9))))

;; https://www.brainm.com/software/pubs/math/Rotation_matrix.pdf
;; angle in [-1, 2]
(define (make-rotation-matrix axis angle)
  (let* ((hpi (asin 1))   ; pi/2
         (cos (cos (* angle hpi)))
         (sin (sin (* angle hpi))))
    (case axis
      ((x) (3x3-matrix 1 0 0 0 cos (* -1 sin) 0 sin cos))
      ((y) (3x3-matrix cos 0 sin 0 1 0 (* -1 sin) 0 cos))
      ((z) (3x3-matrix cos (* -1 sin) 0 sin cos 0 0 0 1))
      (else
       (error "invalid axis" axis)))))      
(assert (equal? '(3 . 3) (matrix-dimensions (make-rotation-matrix 'x 1))))

;; x-angle = heading = -1|0|1|2 (x pi/2)
;; y-angle = bank
;; z-angle = attitude
(define-memoized (rotation-matrix x-angle y-angle z-angle)
  (let ((rot-x (make-rotation-matrix 'x x-angle))
        (rot-y (make-rotation-matrix 'y y-angle))
        (rot-z (make-rotation-matrix 'z z-angle)))
    (matrix-mult (matrix-mult rot-x rot-y) rot-z)))

;; angle = '(x-angle y-angle z-angle)
(define (beacon/rotate b angle)
  (apply make-beacon
         (map (compose round inexact->exact)
              (vector->list
               (matrix-mult-vector
                (rotation-matrix (first angle)
                                 (second angle)
                                 (third angle)) b)))))
(assert (equal? '#(1 -3 2) (beacon/rotate (make-beacon 1 2 3) '(1 0 0))))
;; rotation conservates distance
(assert (equal? (beacon/distance (car (get-scanner 0))
                                 (car (get-scanner 1)))
                (beacon/distance (beacon/rotate (car (get-scanner 0)) '(1 1 1))
                                 (beacon/rotate (car (get-scanner 1)) '(1 1 1)))))

;; group of 24 rotations of the cube (unit = pi/2)
;; https://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
(define *cube-rotations* ;;(possibilities '(-1 0 1 2) 3))
  '( ( 0  0  0 ) ( 1  0  0 ) ( 2  0  0 ) ( -1  0  0 )
     ( 0  1  0 ) ( 1  1  0 ) ( 2  1  0 ) ( -1  1  0 )
     ( 0 -1  0 ) ( 1 -1  0 ) ( 2 -1  0 ) ( -1 -1  0 )
     ( 0  0  1 ) ( 1  0  1 ) ( 2  0  1 ) ( -1  0  1 )
     ( 0  0  2 ) ( 1  0  2 ) ( 2  0  2 ) ( -1  0  2 ) 
     ( 0  0 -1 ) ( 1  0 -1 ) ( 2  0 -1 ) ( -1  0 -1 ) ))

(define (almost=? d1 d2) (= (abs (- d1 d2)) 0));0.001))

;; find the rotation that make pairs of beacons with common distance match
;; (both rotated beacons have same 1-1 distance to beacons from the other pair)
;; rotation applies to the second scanner
(define (find-rotation scan1 scan2)
  (let* ((dist (equal-distances (get-distances scan1)
                                (get-distances scan2))))
    (if (null? dist) #f
        (call/cc
         (lambda (return)
           (let loop ((dist dist))
             (when (null? dist)
                 (return #f))
             (let ((d1 (caar dist)) (d2 (cdar dist)))
               (for-each (lambda (rot)
                           (when (almost=? (beacon/distance
                                            (second d1)
                                            (beacon/rotate (second d2) rot))
                                           (beacon/distance
                                            (third d1)
                                            (beacon/rotate (third d2) rot)))
                             (return rot)))
                         *cube-rotations*)
               (loop (cdr dist)))))))))
(assert (equal? '(2 0 2)
                (find-rotation (get-scanner 0) (get-scanner 1))))
(assert (equal? '(1 0 -1)
                (find-rotation (get-scanner 1) (get-scanner 4))))

(define (beacon/diff b1 b2)
  (list->vector (list (- (beacon-x b1) (beacon-x b2))
                      (- (beacon-y b1) (beacon-y b2))
                      (- (beacon-z b1) (beacon-z b2)))))

(define (beacon/translate b trans)
  (make-beacon (+ (beacon-x b) (vector-ref trans 0))
               (+ (beacon-y b) (vector-ref trans 1))
               (+ (beacon-z b) (vector-ref trans 2))))
(assert (equal? (beacon/translate
                 (beacon/rotate (string->beacon "686,422,578") '(2 0 2))
                 '#(68 -1246 -43))                  
                (string->beacon "-618,-824,-621")))

;; find the translation vector that transforms 
;; beacons common to 2 scanner reports
;; translation applies to the second scanner
(define (find-translation scan1 scan2)
  (let* ((dist (equal-distances (get-distances scan1)
                                (get-distances scan2))))
    (call/cc
     (lambda (return)
       (for-each
        (lambda (dist)
          (let ((d1 (car dist)) (d2 (cdr dist)))
            (when (equal? (beacon/diff (second d1) (second d2))
                          (beacon/diff (third d1) (third d2)))
              (return (beacon/diff (second d1) (second d2))))))
        dist)
       (return #f)))))
(assert (equal? '#(68 -1246 -43)
                (find-translation (get-scanner 0)
                                  (map (lambda (b) (beacon/rotate b '(2 0 2)))
                                       (get-scanner 1)))))

(define-structure (transformation (type list))
  translation1
  rotation
  translation2)

(define (beacon/transform b tra)
  (beacon/translate
   (beacon/rotate
    (beacon/translate b (transformation-translation1 tra))
    (transformation-rotation tra))
   (transformation-translation2 tra)))
(assert (equal? (beacon/transform
                 (string->beacon "686,422,578")
                 (make-transformation '#(0 0 0) '(2 0 2) '#(68 -1246 -43)))
                (string->beacon "-618,-824,-621")))
(assert (equal? (beacon/transform
                 (string->beacon "686,422,578")
                 (make-transformation '#(0 0 0) '(0 0 0) '#(0 0 0)))
                 (string->beacon "686,422,578")))

(define (transformation/invert tra)
  (make-transformation (vector-map invert (transformation-translation2 tra))
                       (map invert (transformation-rotation tra))
                       (vector-map invert (transformation-translation1 tra))))
(let ((tra (make-transformation '#(12 13 14) '(2 0 2) '#(68 -1246 -43)))
      (b (string->beacon "686,422,578")))
  (assert (equal? b (beacon/transform 
                     (beacon/transform b tra)
                     (transformation/invert tra))))
  (assert (equal? b (beacon/transform 
                     (beacon/transform b (transformation/invert tra))
                     tra))))  

(define (beacon<? b1 b2)
  (cond ((< (beacon-x b1) (beacon-x b2)) #t)
        ((> (beacon-x b1) (beacon-x b2)) #f)
        (else (cond ((< (beacon-y b1) (beacon-y b2)) #t)
                    ((> (beacon-y b1) (beacon-y b2)) #f)
                    (else (< (beacon-z b1) (beacon-z b2)))))))

;; return sorted list of beacons common to 2 scanners
;; beacons are relative to first scanner
(define (find-common-beacons scan1 scan2)
  (or
   (and-let* ((rot (find-rotation scan1 scan2))
              (rot)
              (rotated (map (lambda (b) (beacon/rotate b rot)) scan2))
              (tra (find-translation scan1 rotated))
              (tra)
              (translated (map (lambda (b) (beacon/translate b tra)) rotated)))
             (intersect scan1 translated beacon<?))
   '()))
(assert (= 12 (length (find-common-beacons (get-scanner 0) (get-scanner 1)))))
(assert (= 12 (length (find-common-beacons (get-scanner 4) (get-scanner 1)))))
(assert (= 0 (length (find-common-beacons (get-scanner 3) (get-scanner 0)))))

;; find transformation (rotation+translation)
;; that convert beacons from scanner 1 to scanner 2
;; (assuming that scanner 1 and scanner 2 overlap)
(define (find-transformation scan1 scan2)
  (and-let* ((rot (find-rotation scan1 scan2))
             (rot)
             (rotated (map (lambda (b) (beacon/rotate b rot)) scan2))
             (tra (find-translation scan1 rotated))
             (tra)
             (translated (map (lambda (b) (beacon/translate b tra)) rotated))
             (overlap (intersect scan1 translated beacon<?)))
            (cond ((zero? (length overlap)) #f)
                  ((>= 12 (length overlap)) (make-transformation '#(0 0 0) rot tra))
                  (else
                   (warn "overlap<12" (length overlap))
                   #f))))
(assert (find-transformation (get-scanner 0) (get-scanner 1)))
(assert (find-transformation (get-scanner 4) (get-scanner 1)))
(assert (not (find-transformation (get-scanner 3) (get-scanner 0))))

;; matrix of transformations (row -> col)
(define (build-transformation-matrix scanners)
  (fold
   (lambda (i mat)
     (do ((j (1+ i) (1+ j)))
         ((>= j (vector-length scanners)) mat)
       ;; find transformation from group j to i
       (and-let* ((tra (find-transformation (vector-ref scanners i)
                                            (vector-ref scanners j)))
                  (tra))
                 (warn "overlap:" j i)
                 (set! mat (matrix-grow-set! mat j i tra))
                 (set! mat (matrix-grow-set! mat i j (transformation/invert tra))))))                 
   (make-matrix 0 0 #f)
   (iota (vector-length scanners))))
(assert (equal? '(5 . 5) (matrix-dimensions (build-transformation-matrix *test-scanners-2*))))

(load "astar.scm")

;; find list of transformations pathing from i to 0
(define (transformation-sequence mat i)
  (define (hash-node n . bound) (modulo n (car bound)))
  (define (node-edges i)
    (fold (lambda (j ls) (if (matrix-ref mat i j)
                             (cons (make-map-edge i j 1 1) ls)
                             ls))
          '() (iota (matrix-width mat))))
  (define (constant x) 1)
  (define (display-edge e)
    (format #t "[~A->~A]" (map-edge:src e) (map-edge:dst e)))
  ;; find path using A*
  (and-let* ((path (a* #t i = hash-node node-edges identity constant))
             (path))
            (fold-right (lambda (e ls)
                          (cons (matrix-ref mat (map-edge:src e)
                                            (map-edge:dst e)) ls))
                        '() path)))
(let ((mat (build-transformation-matrix *test-scanners-2*)))
  (assert (= 1 (length (transformation-sequence mat  1))))
  (assert (equal? '(#(0 0 0) (2 0 2) #(68 -1246 -43))
                  (car (transformation-sequence mat 1))))
  (assert (= 2 (length (transformation-sequence mat 3)))))

;; transform all beacons relatively to scanner 0
(define (extract-beacons mat scanners)
  (define (transform b seq)
    (fold (lambda (tra b) (beacon/transform b tra))
          b seq))
  (set->list
   (fold
    (lambda (j set)
      (and-let* ((seq (transformation-sequence mat j))
                 (seq))
                ;; transform and insert in the set all beacons from group j
                (for-each (lambda (b) (set/insert! set (transform b seq)))
                          (vector-ref scanners j)))
      set)
    ;; set of resulting beacons (relative to scanner 0)
    (list->set (vector-ref scanners 0))
    (iota (-1+ (vector-length scanners)) 1))))
(assert (= 79 (length (extract-beacons
                       (build-transformation-matrix *test-scanners-2*)
                       *test-scanners-2*))))

;; part 1
(define *input-scanners* (read-scanners (load-file "day_19_input.txt")))
(define *input-matrix* (build-transformation-matrix *input-scanners*))
(length (extract-beacons *input-matrix* *input-scanners*)) 
