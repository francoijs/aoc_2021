;; https://adventofcode.com/2021/day/8

(load "utils.scm")

(define-record-type :display-entry
  (make-display-entry signals patterns)
  display-entry?
  (signals display-entry:signals)
  (patterns display-entry:patterns))

(define (string->pattern str)
  (list->string (map integer->char (sort (map char->integer (string->list str)) <))))
(define (read-display-entry str)
  (let ((tokens (string-split str " |")))
    (make-display-entry (map string->pattern (list-head tokens 10))
                        (map string->pattern (list-tail tokens 10)))))

(let ((entry (read-display-entry "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe")))
  (assert (equal? "be" (first (display-entry:signals entry))))
  (assert (equal? "bde" (tenth (display-entry:signals entry))))
  (assert (equal? "abcdefg" (first (display-entry:patterns entry))))
  (assert (equal? "bceg" (fourth (display-entry:patterns entry))))
  )


;; count patterns with given lengths
(define (count-digits patterns lengths)
  (fold (lambda (pattern count)
          (if (member (string-length pattern) lengths)
              (1+ count)
              count))
        0 patterns))
(assert (= 3 (count-digits '("abc" "ab" "abcd" "") '(0 2 3))))


;; part 1
(assert (= 272 (count-digits (apply append
                                    (map display-entry:patterns
                                         (map read-display-entry
                                              (load-file "day_8_input.txt"))))
                             '(2 4 3 7))))


(define *digits-dictionary*
  '#("abcefg"   ; 0
     "cf"       ; 1 *
     "acdeg"    ; 2
     "acdfg"    ; 3
     "bcdf"     ; 4 *
     "abdfg"    ; 5
     "abdefg"   ; 6
     "acf"      ; 7 *
     "abcdefg"  ; 8 *
     "abcdfg"   ; 9     
     ))

(define (strings-with-length ls len)
  (filter (lambda (s) (= len (string-length s))) ls))
(define (first-string-with-length ls len)
  (car (strings-with-length ls len)))
(assert (equal? "abc" (first-string-with-length '("a" "abc" "bc" "bcd") 3)))
(define (first-char-not-in str chars)
  (find (lambda (c) (not (member c (string->list chars)))) (string->list str)))
(assert (equal? #\a (first-char-not-in "caf" "cf")))
(define (has-char? str char)
  (not (first-char-not-in (list->string (list char)) str)))
(assert (has-char? "abc" #\a))
(assert (not (has-char? "abc" #\d)))
(define (remove-chars str chars)
  (list->string (remove (lambda (c) (has-char? chars c)) (string->list str))))
(assert (equal? "bd" (remove-chars "abcd" "ac")))

;; decode the 10 signals of an entry, and return a dictionary pattern->digit
(define (translate-signals signals)
  (let* ((one (first-string-with-length signals 2))
         (four (first-string-with-length signals 4))
         (seven (first-string-with-length signals 3))
         (eight (first-string-with-length signals 7))
         (a (first-char-not-in seven one))
         (three (car (filter (lambda (str) (= 2 (string-length (remove-chars str seven))))
                             (strings-with-length signals 5))))
         (nine (car (filter (lambda (str) (= 1 (string-length (remove-chars str three))))
                            (strings-with-length signals 6))))
         (b (first-char-not-in nine three))
         (e (first-char-not-in eight nine))
         (five (car (filter (lambda (str) (has-char? str b))
                            (strings-with-length signals 5))))
         (c (first-char-not-in one five))
         (two (car (list-transform-negative (strings-with-length signals 5)
                     (lambda (str) (member str (list three five))))))
         (f (first-char-not-in one two))
         (zero (car (filter (lambda (str) (and (not (string=? str nine)) (has-char? str c)))
                            (strings-with-length signals 6))))
         (d (first-char-not-in eight zero))
         (g (first-char-not-in eight (list->string (list a b c d e f))))
         (six (car (filter (lambda (str) (and (not (string=? str nine)) (not (string=? str zero))))
                           (strings-with-length signals 6))))
         (dic (make-equal-hash-table)))
    (hash-table/put! dic zero 0)
    (hash-table/put! dic one 1)
    (hash-table/put! dic two 2)
    (hash-table/put! dic three 3)
    (hash-table/put! dic four 4)
    (hash-table/put! dic five 5)
    (hash-table/put! dic six 6)
    (hash-table/put! dic seven 7)
    (hash-table/put! dic eight 8)
    (hash-table/put! dic nine 9)
    dic))

(let ((dic (translate-signals '("acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"))))
  (assert (= 8 (hash-table/get dic "acedgfb" #f)))
  (assert (= 5 (hash-table/get dic "cdfbe" #f)))
  (assert (= 2 (hash-table/get dic "gcdfa" #f)))
  (assert (= 3 (hash-table/get dic "fbcad" #f)))
  (assert (= 7 (hash-table/get dic "dab" #f)))
  (assert (= 9 (hash-table/get dic "cefabd" #f)))
  (assert (= 6 (hash-table/get dic "cdfgeb" #f)))
  (assert (= 4 (hash-table/get dic "eafb" #f)))
  (assert (= 0 (hash-table/get dic "cagedb" #f)))
  (assert (= 1 (hash-table/get dic "ab" #f)))
  )

(define (list->number ls)
  (car (fold (lambda (n state) (cons (+ (* n (expt 10 (cdr state))) (car state)) (1+ (cdr state))))
             (cons 0 0)
             (reverse ls))))
(assert (= 1234 (list->number '(1 2 3 4))))

(define (entry->number entry)
  (let ((dic (translate-signals (display-entry:signals entry))))
    (list->number (map (lambda (pattern) (hash-table/get dic pattern #f))
                       (display-entry:patterns entry)))))
(assert (= 5353 (entry->number (read-display-entry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))))

(assert (= 61229 (apply + (map entry->number
                               (map read-display-entry
                                    '("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                                      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
                                      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
                                      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
                                      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
                                      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
                                      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
                                      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
                                      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
                                      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
                                    )))))


;; part 2
(assert (= 1007675 (apply + (map entry->number
                                 (map read-display-entry
                                      (load-file "day_8_input.txt"))))))
