;; https://adventofcode.com/2021/day/14

(load "utils.scm")

(define-record-type :polymerization-manual
  (make-polymerization-manual template rules)
  polymerization-manual?
  (template polymerization-manual:template)
  (rules polymerization-manual:rules))

;; template is a list of chars
;; rules is a hash-table (char . char) -> char
(define (read-polymerization-manual lines)
  (let* ((template (string->list (car lines)))
         (rules (fold (lambda (line dic) (let* ((tokens (string-split line " ->"))
                                                (pair (cons (string-ref (first tokens) 0)
                                                            (string-ref (first tokens) 1))))
                                           (hash-table/put! dic pair (car (string->list (second tokens))))
                                           dic))
                      (make-equal-hash-table)
                      (cddr lines))))
    (make-polymerization-manual template rules)))

(define *test-manual* (read-polymerization-manual '("NNCB"
                                                    ""
                                                    "CH -> B"
                                                    "HH -> N"
                                                    "CB -> H"
                                                    "NH -> C"
                                                    "HB -> C"
                                                    "HC -> B"
                                                    "HN -> C"
                                                    "NN -> C"
                                                    "BH -> H"
                                                    "NC -> B"
                                                    "NB -> B"
                                                    "BN -> B"
                                                    "BB -> N"
                                                    "BC -> B"
                                                    "CC -> N"
                                                    "CN -> C")))
(assert (equal? '(#\N #\N #\C #\B) (polymerization-manual:template *test-manual*)))
(assert (= 16 (hash-table/count (polymerization-manual:rules *test-manual*))))

;; pair-distri is a hash-table of (char . char) -> count
(define (polymer-pair-distribution polymer)
  (let loop ((left-elem (car polymer))
             (right-elem (cadr polymer))
             (rest (cddr polymer))
             (distri (make-equal-hash-table)))
    (hash-table/inc! distri (cons left-elem right-elem))
    (if (null? rest) distri
        (loop right-elem (car rest) (cdr rest) distri))))
(assert (equal? '(((#\C . #\B) . 1) ((#\N . #\C) . 1) ((#\N . #\N) . 1))
                (hash-table->alist (polymer-pair-distribution (polymerization-manual:template *test-manual*)))))

(define (pair-distribution/step distri rules)
  (fold (lambda (pair distri)
          (let* ((left-elem (car (car pair)))
                 (right-elem (cdr (car pair)))
                 (new-elem (hash-table/get rules (car pair) #f)))
            (if new-elem
                (begin
                  (hash-table/add! distri (car pair) (- 0 (cdr pair)))
                  (hash-table/add! distri (cons left-elem new-elem) (cdr pair))
                  (hash-table/add! distri (cons new-elem right-elem) (cdr pair))))
            distri))
        (hash-table/copy distri)
        (hash-table->alist distri)))
(assert (= 6 (length (list-transform-negative
                         (hash-table->alist (pair-distribution/step
                                             (polymer-pair-distribution (polymerization-manual:template *test-manual*))
                                             (polymerization-manual:rules *test-manual*)))
                        (lambda (p) (zero? (cdr p)))))))

;; distri is a list of pairs (char . count)
(define (pair-distribution->distribution pdistri first-elem last-elem)
  (let ((distri 
         (fold (lambda (pair distri)
;;                 (warn pair)
                 (let* ((left-elem (car (car pair)))
                        (right-elem (cdr (car pair))))
                   (hash-table/add! distri left-elem (cdr pair))
                   (hash-table/add! distri right-elem (cdr pair))
                   distri))
               (make-equal-hash-table)
               (hash-table->alist pdistri))))
    (hash-table/add! distri first-elem 1)
    (hash-table/add! distri last-elem 1)
    (map (lambda (pair) (cons (car pair) (/ (cdr pair) 2)))
         (hash-table->alist distri))))
(assert (equal? '((#\N . 2) (#\C . 1) (#\B . 1))
                (pair-distribution->distribution
                 (polymer-pair-distribution (polymerization-manual:template *test-manual*))
                 #\N #\B)))

;; return distribution of the polymer after n steps ; sorted by decreasing count
(define (polymerize manual steps)
  (let ((distri (pair-distribution->distribution
                 (fold (lambda (step distri)
                         (pair-distribution/step distri (polymerization-manual:rules manual)))
                       (polymer-pair-distribution (polymerization-manual:template manual))
                       (iota steps))
                 (first (polymerization-manual:template manual))
                 (last (polymerization-manual:template manual)))))
    (sort distri (lambda (a b) (> (cdr a) (cdr b))))))
(assert (= 3073 (apply + (map cdr (polymerize *test-manual* 10)))))


;; part 1
(let ((distri (polymerize (read-polymerization-manual (load-file "day_14_input.txt")) 10)))
  (assert (= 4244 (- (cdr (first distri)) (cdr (last distri))))))


;; part 2
(let ((distri (polymerize (read-polymerization-manual (load-file "day_14_input.txt")) 40)))
  (assert (= 4807056953866 (- (cdr (first distri)) (cdr (last distri))))))


;; old slow code for part 1

;; (define (polymerization/step polymer rules)
;;   (let loop((head (car polymer))
;;             (rest (cdr polymer))
;;             (polymer '()))
;; ;;    (warn head rest polymer)
;;     (if (null? rest) (reverse (cons head polymer))
;;         (let* ((pair (cons head (car rest)))
;;                (elem (hash-table/get rules pair #f)))
;;           (loop (car rest)
;;                 (cdr rest)
;;                 (if elem
;;                     (cons elem (cons head polymer))
;;                     (cons head polymer)))))))
;; (assert (string=? "NCNBCHB" (list->string (polymerization/step (polymerization-manual:template *test-manual*)
;;                                                                (polymerization-manual:rules *test-manual*)))))
;; (assert (string=? "NBCCNBBBCBHCB" (list->string (polymerization/step (string->list "NCNBCHB")
;;                                                                      (polymerization-manual:rules *test-manual*)))))
;; (assert (string=? "NBBBCNCCNBBNBNBBCHBHHBCHB" (list->string (polymerization/step (string->list "NBCCNBBBCBHCB")
;;                                                                                  (polymerization-manual:rules *test-manual*)))))

;; (define (polymerize manual steps)
;;   (fold (lambda (step distri) (pair-distribution/step distri
;;                                                       (polymerization-manual:rules manual)))
;;         (polymer-pair-distribution (polymerization-manual:template manual))
;;         (iota steps)))
;; (assert (= 3073 (length (polymerize *test-manual* 10))))

;; ;; return list of pairs (char . count), sorted by decreasing count
;; (define (polymer-distribution polymer)
;;   (let loop ((ls (sort polymer char<?))
;;              (distribution '()))
;;     (if (not ls)
;;         (sort distribution (lambda (a b) (> (cdr a) (cdr b))))
;;         (let* ((next-elem (list-search-negative ls (lambda (elem) (equal? elem (car ls))))))
;;           (loop (member next-elem ls)
;;                 (cons (cons (car ls) (length (member (car ls) (reverse ls))))
;;                       distribution))))))
;; (assert (equal? '(#\B . 1749) (first (polymer-distribution (polymerize *test-manual* 10)))))
;; (assert (equal? '(#\H . 161) (last (polymer-distribution (polymerize *test-manual* 10)))))

;; ;; part 1
;; (let ((distri (polymer-distribution (polymerize (read-polymerization-manual (load-file "day_14_input.txt")) 10))))
;;   (assert (= 4244 (- (cdr (first distri)) (cdr (last distri))))))
