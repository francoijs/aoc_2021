;; https://adventofcode.com/2021/day/14

(load "utils.scm")

(define-record-type :polymerization-manual
  (make-polymerization-manual template rules)
  polymerization-manual?
  (template polymerization-manual:template)
  (rules polymerization-manual:rules))

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

(define (polymerization/step polymer rules)
  (let loop((head (car polymer))
            (rest (cdr polymer))
            (polymer '()))
;;    (warn head rest polymer)
    (if (null? rest) (reverse (cons head polymer))
        (let* ((pair (cons head (car rest)))
               (elem (hash-table/get rules pair #f)))
          (loop (car rest)
                (cdr rest)
                (if elem
                    (cons elem (cons head polymer))
                    (cons head polymer)))))))
(assert (string=? "NCNBCHB" (list->string (polymerization/step (polymerization-manual:template *test-manual*)
                                                               (polymerization-manual:rules *test-manual*)))))
(assert (string=? "NBCCNBBBCBHCB" (list->string (polymerization/step (string->list "NCNBCHB")
                                                                     (polymerization-manual:rules *test-manual*)))))
(assert (string=? "NBBBCNCCNBBNBNBBCHBHHBCHB" (list->string (polymerization/step (string->list "NBCCNBBBCBHCB")
                                                                                 (polymerization-manual:rules *test-manual*)))))

(define (polymerize manual steps)
  (fold (lambda (step polymer) (polymerization/step polymer
                                                    (polymerization-manual:rules manual)))
        (polymerization-manual:template manual)
        (iota steps)))
(assert (= 3073 (length (polymerize *test-manual* 10))))

;; return list of pairs (char . count), sorted by decreasing count
(define (polymer-distribution polymer)
  (let loop ((ls (sort polymer char<?))
             (distribution '()))
    (if (not ls)
        (sort distribution (lambda (a b) (> (cdr a) (cdr b))))
        (let* ((next-elem (list-search-negative ls (lambda (elem) (equal? elem (car ls))))))
          (loop (member next-elem ls)
                (cons (cons (car ls) (length (member (car ls) (reverse ls))))
                      distribution))))))
(assert (equal? '(#\B . 1749) (first (polymer-distribution (polymerize *test-manual* 10)))))
(assert (equal? '(#\H . 161) (last (polymer-distribution (polymerize *test-manual* 10)))))

;; part 1
(let ((distri (polymer-distribution (polymerize (read-polymerization-manual (load-file "day_14_input.txt")) 10))))
  (assert (= 4244 (- (cdr (first distri)) (cdr (last distri))))))
