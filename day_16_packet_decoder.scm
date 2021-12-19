;; https://adventofcode.com/2021/day/16

(load "utils.scm")

;; binary is a list of 0 or 1's
(define (hexa->binary str)
  (let* ((bin (map (lambda (c) (char->digit c 2))
                  (string->list (number->string (string->number str 16) 2))))
         (mod (modulo (length bin) 4)))
    (append (if (> mod 0) (make-list (- 4 mod) 0) '()) bin)))
(assert (equal? (make-list 8 1) (hexa->binary "ff")))
(assert (equal? '(0 0 1 1 1 0 0 0) (hexa->binary "38")))

(define (binary->number ls)
  (string->number (list->string (map (lambda (d) (digit->char d 2)) ls)) 2))
(assert (= 10 (binary->number '(1 0 1 0))))

;; return (number . rest)
(define (read-number ls len)
  (cons (binary->number (list-head ls len))
        (list-tail ls len)))

;; return (version . rest)
(define (read-version ls) (read-number ls 3))
(assert (equal? '(6 0 1 0) (read-version '(1 1 0 0 1 0))))

;; return (type-id . rest)
(define (read-type-id ls) (read-number ls 3))
(assert (equal? '(4 0 1 0) (read-type-id '(1 0 0 0 1 0))))

;; return (value . rest)
(define (read-literal-value ls)
  (let loop ((ls ls) (val 0) (last #f))
    (if last (cons val ls)
        (loop (list-tail ls 5)
              (+ (* 16 val)
                 (binary->number (list-head (list-tail ls 1) 4)))
              (if (= 0 (car ls)) #t #f)))))
(assert (equal? '(2021 0 0 0) (read-literal-value '(1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0))))

;; return ((bits | count . number) . rest)
(define (read-sub-packets-info ls)
  (let ((length-type-id (car ls)))
    (if (= 0 length-type-id)
        (let ((number (read-number (cdr ls) 15)))
          (cons (cons 'sub-packets-bits (car number)) (cdr number)))
        (let ((number (read-number (cdr ls) 11)))
          (cons (cons 'sub-packets-count (car number)) (cdr number))))))
(assert (equal? '(sub-packets-bits . 27)
                (car (read-sub-packets-info '(0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0)))))
(assert (equal? '(sub-packets-count . 3)
                (car (read-sub-packets-info '(1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0)))))

;; return (list of pkts . rest)
(define (read-operator ls)
  (let* ((info (read-sub-packets-info ls)))
    (if (eq? 'sub-packets-bits (caar info))
        (let ((pkts (read-packets (list-head (cdr info) (cdar info)) #f)))
          (cons (car pkts) (list-tail (cdr info) (cdar info))))
        (read-packets (cdr info) (cdar info)))))
(assert (= 2 (length (car (read-operator '(0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0))))))
(assert (= 7 (length (cdr (read-operator '(0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0))))))   ; 7 bits left
(assert (= 3 (length (car (read-operator '(1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0))))))
(assert (= 5 (length (cdr (read-operator '(1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0))))))   ; 5 bits left

(define +type-id-sum+ 0)
(define +type-id-product+ 1)
(define +type-id-min+ 2)
(define +type-id-max+ 3)
(define +type-id-literal-value+ 4)
(define +type-id-gt+ 5)
(define +type-id-lt+ 6)
(define +type-id-equal+ 7)

(define-record-type :bits-packet
  (make-bits-packet version type-id payload)
  bits-packet?
  (version bits-packet:version)
  (type-id bits-packet:type-id)
  (payload bits-packet:payload))
(define (display-bits-packet bp)
  (format #t "[ver~A typ~A ~A]"
          (bits-packet:version bp) (bits-packet:type-id bp) (bits-packet:payload bp)))

;; read at most count packets (or as many as possible if count = #f)
;; return (list of pkts . rest)
(define (read-packets ls count)
  (if (or (< (length ls) 4)
          (and count (zero? count)))
      (cons '() ls)
      (let* ((pkt (read-packet ls))
             (rest (read-packets (cdr pkt)
                                 (if count (-1+ count) #f))))
;;        (display-bits-packet (car pkt))
        (cons (cons (car pkt) (car rest))
              (cdr rest)))))
(assert (= 1 (length (car (read-packets '(1 1 0 1 0 0 0 1 0 1 0 0 0) 1)))))
(assert (= 1 (length (car (read-packets '(0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0) 1)))))
(assert (= 2 (length (car (read-packets '(1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 1) 2)))))

;; return (packet . rest)
(define (read-packet ls)
  (let* ((version (read-version ls))
         (type-id (read-type-id (cdr version))))
    (if (= (car type-id) +type-id-literal-value+)
        (let ((value (read-literal-value (cdr type-id))))
          (cons (make-bits-packet (car version) (car type-id) (car value))
                (cdr value)))
        (let ((operator (read-operator (cdr type-id))))
          (cons (make-bits-packet (car version) (car type-id) (car operator))
                (cdr operator))))))
(assert (= 4 (length (read-packet (hexa->binary "D2FE28")))))   ; 1 pkt and 3 bits left
(assert (= 8 (length (read-packet (hexa->binary "38006F45291200")))))   ; 1 pkt and 7 bits left
(assert (= 6 (length (read-packet (hexa->binary "EE00D40C823060")))))   ; 1 pkt and 5 bits left

(define (sum-versions pkt)
  (+ (bits-packet:version pkt)
     (if (= (bits-packet:type-id pkt) +type-id-literal-value+)
         0
         (apply + (map sum-versions (bits-packet:payload pkt))))))
(assert (= 16 (sum-versions (car (read-packet (hexa->binary "8A004A801A8002F478"))))))
(assert (= 12 (sum-versions (car (read-packet (hexa->binary "620080001611562C8802118E34"))))))
(assert (= 23 (sum-versions (car (read-packet (hexa->binary "C0015000016115A2E0802F182340"))))))
(assert (= 31 (sum-versions (car (read-packet (hexa->binary "A0016C880162017C3686B18A3D4780"))))))

;; part 1
(assert (= 821 (chain (load-file "day_16_input.txt")
                      car
                      hexa->binary
                      read-packet
                      car
                      sum-versions)))


(define (type-id->operator tid)
  (case tid
    ((0) +)
    ((1) *)
    ((2) min)
    ((3) max)
    ((4) (lambda (a) a))
    ((5) (lambda (a b) (if (> a b) 1 0)))
    ((6) (lambda (a b) (if (< a b) 1 0)))
    ((7) (lambda (a b) (if (= a b) 1 0)))
    (else
     (error "no operator for type-id" tid))))
(assert (equal? * (type-id->operator 1)))
(assert (equal? max (type-id->operator 3)))

(define (read-structure pkt)
  (let ((id (bits-packet:type-id pkt)))
    (if (= 4 id)
        (bits-packet:payload pkt)
        (cons (type-id->operator id)
              (map read-structure
                   (bits-packet:payload pkt))))))
(assert (= 3 (eval (read-structure (car (read-packet (hexa->binary "C200B40A82")))) (null-environment 5))))
;(eval (read-structure (car (read-packet (hexa->binary "04005AC33890")))) (null-environment 5))
(assert (= 7 (eval (read-structure (car (read-packet (hexa->binary "880086C3E88112")))) (null-environment 5))))
(assert (= 9 (eval (read-structure (car (read-packet (hexa->binary "CE00C43D881120")))) (null-environment 5))))
(assert (= 1 (eval (read-structure (car (read-packet (hexa->binary "D8005AC2A8F0")))) (null-environment 5))))
(assert (= 0 (eval (read-structure (car (read-packet (hexa->binary "F600BC2D8F")))) (null-environment 5))))
(assert (= 0 (eval (read-structure (car (read-packet (hexa->binary "9C005AC2F8F0")))) (null-environment 5))))
(assert (= 1 (eval (read-structure (car (read-packet (hexa->binary "9C0141080250320F1802104A08")))) (null-environment 5))))

;; part 2
(assert (= 2056021084691 (eval (chain (load-file "day_16_input.txt")
                                      car
                                      hexa->binary
                                      read-packet
                                      car
                                      read-structure) (null-environment 5))))
