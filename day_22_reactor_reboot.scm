;; https://adventofcode.com/2021/day/22

(load "utils.scm")

(define-structure (cuboid (type vector)) x1 x2 y1 y2 z1 z2)

;; reboot step = (cuboid . state)
(define (read-reboot-step str)
  (let ((tokens (string-split str "=.,")))
    (cons (make-cuboid (string->number (list-ref tokens 1))
                       (string->number (list-ref tokens 2))
                       (string->number (list-ref tokens 4))
                       (string->number (list-ref tokens 5))
                       (string->number (list-ref tokens 7))
                       (string->number (list-ref tokens 8)))
          (substring? "on" (list-ref tokens 0)))))
(let ((step (read-reboot-step "on x=10..12,y=10..12,z=10..12")))
  (assert (cdr step))
  (assert (= 12 (cuboid-x2 (car step))))
  (assert (= 10 (cuboid-z1 (car step)))))
(let ((step (read-reboot-step "off x=9..11,y=9..11,z=9..11")))
  (assert (not (cdr step)))
  (assert (= 11 (cuboid-z2 (car step))))
  (assert (= 9 (cuboid-x1 (car step)))))

(define-structure bit-cube matrix xs ys zs)

;; Store all possible ranges on 1 axis as a vector of pairs (coord . index)
;; e.g: '(10 11 12) -> '#((10 . 0) (11 . 2) (12 . 4))
;;      index is 0 for 10..10, 1 for 10..11, 2 for 11..11, etc
;; States of ranges are stored in a matrix of bit-strings
(define (build-bit-cube cuboids)
  (define (coords->vector cs)
    (let ((vec (list->vector (uniq (sort cs <)))))
      (fold (lambda (n res)
              (vector-set res n (cons (vector-ref vec n) (* 2 n))))
            (make-vector (vector-length vec))
            (iota (vector-length vec)))))
  (let ((xs (coords->vector (append (map cuboid-x1 cuboids)
                                    (map cuboid-x2 cuboids))))
        (ys (coords->vector (append (map cuboid-y1 cuboids)
                                    (map cuboid-y2 cuboids))))
        (zs (coords->vector (append (map cuboid-z1 cuboids)
                                    (map cuboid-z2 cuboids)))))
    (warn "dimensions: " (vector-length xs) (vector-length ys) (vector-length zs))
    (make-bit-cube 
     (make-matrix (-1+ (* 2 (vector-length xs)))
                  (-1+ (* 2 (vector-length ys)))
                  #f)
     xs ys zs)))

(define (bit-cube-ref cube x y z)
  (let ((bs (matrix-ref (bit-cube-matrix cube) x y)))
    (and bs (bit-string-ref bs z))))

(define *test-steps* (map read-reboot-step
                          '("on x=10..12,y=10..12,z=10..12"
                            "on x=11..13,y=11..13,z=11..13"
                            "off x=9..11,y=9..11,z=9..11"
                            "on x=10..10,y=10..10,z=10..10")))
(let ((bc (build-bit-cube (map car *test-steps*))))
  (assert (equal? '(9 . 9) (matrix-dimensions (bit-cube-matrix bc))))
  (assert (not (bit-cube-ref bc 0 0 0))))

(define (bit-cube-set! cube x y z1 z2)
  (let ((bs (matrix-ref (bit-cube-matrix cube) x y)))
    (if bs
        (bit-string-set-range! bs z1 (1+ z2))
        ;; allocate new bit-string for the z coordinate
        (let ((bs (make-bit-string (-1+ (* 2 (vector-length (bit-cube-zs cube)))) #f)))
          (bit-string-set-range! bs z1 (1+ z2))
          (matrix-set! (bit-cube-matrix cube) x y bs)))))

(define (bit-cube-clear! cube x y z1 z2)
  (let ((bs (matrix-ref (bit-cube-matrix cube) x y)))
    (or (not bs)
        (begin
          (bit-string-clear-range! bs z1 (1+ z2))
          (when (bit-string-zero? bs)
            ;; all bits are #f on the z coordinate ; bit-string may be deallocated
            (matrix-set! (bit-cube-matrix cube) x y #f))))))

;; perform 1 step and return the cube
(define (bit-cube/step! step cube)
  (define-memoized (index-of accessor c)
    (cdr (vector-binary-search (accessor cube) < car c)))
  (let ((x1 (index-of bit-cube-xs (cuboid-x1 (car step))))
        (x2 (index-of bit-cube-xs (cuboid-x2 (car step))))
        (y1 (index-of bit-cube-ys (cuboid-y1 (car step))))
        (y2 (index-of bit-cube-ys (cuboid-y2 (car step))))
        (z1 (index-of bit-cube-zs (cuboid-z1 (car step))))
        (z2 (index-of bit-cube-zs (cuboid-z2 (car step)))))
    (warn x1 x2 y1 y2 z1 z2)
    (let loop ((x x1) (y y1))
      (cond ((= x (1+ x2)) cube)
            ((= y (1+ y2)) (loop (1+ x) y1))
            (else
             ((if (cdr step) bit-cube-set! bit-cube-clear!)
              cube x y z1 z2)
             (loop x (1+ y)))))))
(let* ((step (read-reboot-step "on x=10..12,y=10..12,z=10..12"))
       (bc (build-bit-cube (list (car step)))))
  (assert (equal? '#*111 (matrix-ref (bit-cube-matrix (bit-cube/step! step bc)) 0 0))))

(define (bit-cube/count-ones cube)
  ;; length of the range of coords represented by index i
  ;; e.g: axis = '#((10 . 0) (13 . 2) (14 . 4))
  ;;      0->1 (10), 1->2 (11..12), 2->1 (13), 3->0, 4->1 (14)
  (define (range-length axis i)
    (if (even? i) 1
        (- (car (vector-ref axis (/ (1+ i) 2)))
           (car (vector-ref axis (/ (-1+ i) 2)))
           1)))
  ;; for each cell #t of the cube, sum the 3d volumes of the corresponding coord ranges
  (let ((count 0))
    (matrix-for-each (lambda (x y bs)
                       (let next-one ((z 0))
                         (let ((z (and bs (bit-substring-find-next-set-bit bs z (bit-string-length bs)))))
                           (when z
                             (set! count (+ count (* (range-length (bit-cube-xs cube) x)
                                                     (range-length (bit-cube-ys cube) y)
                                                     (range-length (bit-cube-zs cube) z))))                           
                             (next-one (1+ z))))))
                     (bit-cube-matrix cube))
    count))
(assert (= 39 (bit-cube/count-ones (fold bit-cube/step!
                                         (build-bit-cube (map car *test-steps*))
                                         *test-steps*))))

(define (step-has-max-dimension n)
  (lambda (step)
    (and (<= (abs (cuboid-x1 (car step))) n)
         (<= (abs (cuboid-x2 (car step))) n)
         (<= (abs (cuboid-y1 (car step))) n)
         (<= (abs (cuboid-y2 (car step))) n)
         (<= (abs (cuboid-z1 (car step))) n)
         (<= (abs (cuboid-z2 (car step))) n))))

(define *test-steps-2* (map read-reboot-step
                            '("on x=-20..26,y=-36..17,z=-47..7"
                              "on x=-20..33,y=-21..23,z=-26..28"
                              "on x=-22..28,y=-29..23,z=-38..16"
                              "on x=-46..7,y=-6..46,z=-50..-1"
                              "on x=-49..1,y=-3..46,z=-24..28"
                              "on x=2..47,y=-22..22,z=-23..27"
                              "on x=-27..23,y=-28..26,z=-21..29"
                              "on x=-39..5,y=-6..47,z=-3..44"
                              "on x=-30..21,y=-8..43,z=-13..34"
                              "on x=-22..26,y=-27..20,z=-29..19"
                              "off x=-48..-32,y=26..41,z=-47..-37"
                              "on x=-12..35,y=6..50,z=-50..-2"
                              "off x=-48..-32,y=-32..-16,z=-15..-5"
                              "on x=-18..26,y=-33..15,z=-7..46"
                              "off x=-40..-22,y=-38..-28,z=23..41"
                              "on x=-16..35,y=-41..10,z=-47..6"
                              "off x=-32..-23,y=11..30,z=-14..3"
                              "on x=-49..-5,y=-3..45,z=-29..18"
                              "off x=18..30,y=-20..-8,z=-3..13"
                              "on x=-41..9,y=-7..43,z=-33..15"
                              "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877"
                              "on x=967..23432,y=45373..81175,z=27513..53682")))
(assert (= 590784 (bit-cube/count-ones (fold bit-cube/step!
                                             (build-bit-cube (map car *test-steps-2*))
                                             (filter (step-has-max-dimension 50)
                                                     *test-steps-2*)))))

;; part 1
(let ((steps (filter (step-has-max-dimension 50)
                     (map read-reboot-step
                          (load-file "day_22_input.txt")))))
  (warn "steps:" (length steps))
  (assert (= 648023 (bit-cube/count-ones
                     (fold bit-cube/step!
                           (build-bit-cube (map car steps))
                           steps)))))

;; part 2
(let* ((steps (map read-reboot-step
                   (load-file "day_22_input.txt")))
       (cube (build-bit-cube (map car steps)))
       (count (length steps)))
  (warn "steps:" (length steps))
  (bit-cube/count-ones
   (fold (lambda (step cube)
           (set! count (-1+ count))
           (when (zero? (modulo count 10))
             (warn "steps:" count)
             (print-gc-statistics))
           (bit-cube/step! step cube))
         cube steps)))
