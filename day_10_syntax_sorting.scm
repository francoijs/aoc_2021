;; https://adventofcode.com/2021/day/10

(load "utils.scm")

(define *chunk-chars* '((#\( #\) 3 1)
                        (#\[ #\] 57 2)
                        (#\{ #\} 1197 3)
                        (#\< #\> 25137 4)))
(define (chunk-matching-char char)
  (let ((entry (find (lambda (entry) (or (eq? char (first entry))
                                         (eq? char (second entry))))
                     *chunk-chars*)))
    (cond ((not entry) (error "invalid char " char))
          ((eq? char (first entry)) (second entry))
          (else (first entry)))))
(assert (eq? #\[ (chunk-matching-char #\])))
(assert (eq? #\> (chunk-matching-char #\<)))

(define (chunk-opening-char? char)
  (find (lambda (entry) (eq? char (first entry))) *chunk-chars*))
(define (chunk-closing-char? char)
  (find (lambda (entry) (eq? char (second entry))) *chunk-chars*))

(define (chunk-char-value char)
  (let ((entry (find (lambda (entry) (or (eq? char (first entry))
                                         (eq? char (second entry))))
                     *chunk-chars*)))
    (if entry (third entry) 0)))
(assert (= 1197 (chunk-char-value #\})))
(assert (= 3 (chunk-char-value #\()))
(assert (= 0 (chunk-char-value #f)))

(define (parse-line line)
  (call/cc
   (lambda (return)
     (return (fold (lambda (char stack)
                     (cond ((chunk-opening-char? char) (cons char stack))
                           ((chunk-closing-char? char)
                            (if (eq? (car stack) (chunk-matching-char char))
                                (cdr stack) (return char)))
                           (else
                            (error "invalid char" char))))
                   '()
                   (string->list line))))))

(define (find-syntax-error line)
  (let ((res (parse-line line)))
    (if (char? res) res #f)))
(assert (eq? #\} (find-syntax-error "{([(<{}[<>[]}>{[]{[(<()>")))
(assert (not (find-syntax-error "[({(<(())[]>[[{[]{<()<>>")))

(define *test-lines* '("[({(<(())[]>[[{[]{<()<>>"
                       "[(()[<>])]({[<{<<[]>>("
                       "{([(<{}[<>[]}>{[]{[(<()>"
                       "(((({<>}<{<{<>}{[]{[]{}"
                       "[[<[([]))<([[{}[[()]]]"
                       "[{[{({}]{}}([{[{{{}}([]"
                       "{<[[]]>}<{[{[{[]{()[[[]"
                       "[<(<(<(<{}))><([]([]()"
                       "<{([([[(<>()){}]>(<<{{"
                       "<{([{{}}[<[[[<>{}]]]>[]]"))
(assert (= 26397 (apply + (map (compose chunk-char-value find-syntax-error) *test-lines*))))

;; part 1
(assert (= 311895 (apply + (map (compose chunk-char-value find-syntax-error)
                                (load-file "day_10_input.txt")))))


(define (chunk-char-value-2 char)
  (let ((entry (find (lambda (entry) (or (eq? char (first entry))
                                         (eq? char (second entry))))
                     *chunk-chars*)))
    (if entry (fourth entry) 0)))
(assert (= 3 (chunk-char-value-2 #\{)))

(define (autocomplete-score line)
  (let ((stack (parse-line line)))
    (if (char? stack) 0
        (fold (lambda (score sum) (+ score (* 5 sum)))
              0
              (map chunk-char-value-2 stack)))))
(assert (= 288957 (autocomplete-score "[({(<(())[]>[[{[]{<()<>>")))
(assert (= 294 (autocomplete-score "<{([{{}}[<[[[<>{}]]]>[]]")))

;; part 2
(let ((scores (filter positive? (map autocomplete-score (load-file "day_10_input.txt")))))
  (assert (= 2904180541 (list-ref (sort scores <) (quotient (length scores) 2)))))
