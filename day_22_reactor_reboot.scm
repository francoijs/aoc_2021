;; https://adventofcode.com/2021/day/22

(load "utils.scm")

;; bit-cube = matrix of bit-strings
(define (make-bit-cube side)
  (build-matrix side side
                (lambda (r c) (make-bit-string side #f)))))

(define-structure cuboid x1 x2 y1 y2 z1 z2)

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

;; perform 1 step and return the cube
(define (bit-cube/step! step cube)
  (define (idx pos)
    (+ pos (quotient (-1+ (matrix-height cube)) 2)))
  (let ((cuboid (car step)))
    (matrix-for-each
     (lambda (row col bs)
       ((if (cdr step)
            bit-string-set-range!
            bit-string-clear-range!)
        bs
        (idx (cuboid-z1 cuboid))
        (1+ (idx (cuboid-z2 cuboid)))))
     (submatrix cube
                (idx (cuboid-x1 cuboid))
                (idx (cuboid-y1 cuboid))
                (- (idx (cuboid-x2 cuboid)) (idx (cuboid-x1 cuboid)) -1)
                (- (idx (cuboid-y2 cuboid)) (idx (cuboid-y1 cuboid)) -1))))
  cube)

;; count bits 1
(define (bit-cube/count-ones cube)
  (matrix-fold (lambda (bs sum) (+ sum (bit-string-count-ones bs)))
               0
               cube))
(assert (= 27 (bit-cube/count-ones
               (bit-cube/step!
                (read-reboot-step "on x=10..12,y=10..12,z=10..12")
                (make-bit-cube 24)))))

(define *test-steps* (map read-reboot-step
                          '("on x=10..12,y=10..12,z=10..12"
                            "on x=11..13,y=11..13,z=11..13"
                            "off x=9..11,y=9..11,z=9..11"
                            "on x=10..10,y=10..10,z=10..10")))
(assert (= 39 (bit-cube/count-ones (fold bit-cube/step!
                                         (make-bit-cube 101)
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
                                             (make-bit-cube 101)
                                             (filter (step-has-max-dimension 50)
                                                     *test-steps-2*)))))

;; part 1
(assert (= 648023 (bit-cube/count-ones
                   (fold bit-cube/step!
                         (make-bit-cube 101)
                         (filter (step-has-max-dimension 50)
                                 (map read-reboot-step
                                      (load-file "day_22_input.txt")))))))
