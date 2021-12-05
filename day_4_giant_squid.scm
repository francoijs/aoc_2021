;; https://adventofcode.com/2021/day/4

(load "utils.scm")

;; parse lines of a grid and return a vector of the locations of each number (=index)
;; the format of a location is '(grid-id line col)
(define (grid->locations lines grid-id)
  (let ((locations (make-vector 0)))
    (let parse-lines((lines lines) (line 0))
      (if (null? lines) locations
          (begin
            (let parse-numbers((numbers (map string->number (string-split (car lines) " "))) (column 0))
              (unless (null? numbers)
                (let ((num (car numbers))
                      (loc (list grid-id line column)))
                  (begin
                    (set! locations (vector-grow-set! locations num loc))
                    (parse-numbers (cdr numbers) (1+ column))))))
            (parse-lines (cdr lines) (1+ line)))))))

(let ((locs (grid->locations '("22 13 17 11  0"
                               " 8  2 23  4 24"
                               "21  9 14 16  7"
                               " 6 10  3 18  5"
                               " 1 12 20 15 19") 42)))
  (assert (= 25 (vector-length locs)))
  (assert (equal? '(42 1 2) (vector-ref locs 23))))

;; merge 2 vectors in a vector of lists containing elements of both vectors
;; (vector1 items may be only #f or lists of elements)
(define (merge-vectors! vector1 vector2)
  (let ((vector1 (if (< (vector-length vector1) (vector-length vector2))
                       (vector-grow vector1 (vector-length vector2))
                       vector1)))
    (let add-elements((ls (vector->list vector2)) (index 0))
      (if (null? ls) vector1
          (let ((elt2 (car ls))
                (elt1 (vector-ref vector1 index)))
            (vector-set! vector1 index
                         (append (if elt1 elt1 '())
                                 (if elt2 (list elt2) '())))
            (add-elements (cdr ls) (1+ index)))))))

(assert (equal? (merge-vectors! '#((0) (1 2)) '#(1 3 0))
                '#((0 1) (1 2 3) (0))))

;; read grids described by a list of strings
;; return a vector giving all the locations of each number
(define (read-grids lines)
  (let ((grids (list-split lines "")))
    (let parse-grids((grids grids) (grid-id 0))
      (if (null? grids) (make-vector 0)
          (merge-vectors! (parse-grids (cdr grids) (1+ grid-id))
                          (grid->locations (car grids) grid-id))))))

(define test-grids '("22 13 17 11  0"
                     " 8  2 23  4 24"
                     "21  9 14 16  7"
                     " 6 10  3 18  5"
                     " 1 12 20 15 19"
                     ""
                     " 3 15  0  2 22"
                     " 9 18 13 17  5"
                     "19  8  7 25 23"
                     "20 11 10 24  4"
                     "14 21 16 12  6"
                     ""
                     "14 21 17 24  4"
                     "10 16 15  9 19"
                     "18  8 23 26 20"
                     "22 11 13  6  5"
                     " 2  0 12  3  7"))
(assert (= 27 (vector-length (read-grids test-grids))))
(assert (equal? '((2 3 0) (1 0 4) (0 0 0)) (vector-ref (read-grids test-grids) 22)))


;; return an iterator which take a list of numbers in arguments,
;; and yields the next winning grid and the remaining numbers (including the winning one)
(define (winning-grids-iterator locations)
  (let ((grids (make-equal-hash-table))
        (won-grid (make-equal-hash-table)))
    (lambda (numbers)
      (call/cc
       (lambda (yield)
         (let play-numbers((numbers numbers))
           (if (null? numbers)
               (error "no numbers left!")
               (let update-grids((ls (vector-ref locations (car numbers))))
                 (unless (null? ls)
                   (let* ((num (car numbers))
                          (loc (car ls))
                          (grid (hash-table/get grids (first loc) (cons (make-vector 5 5) (make-vector 5 5)))))
                     (vector-dec! (car grid) (second loc))
                     (vector-dec! (cdr grid) (third loc))
                     (if (or (zero? (vector-ref (car grid) (second loc)))
                             (zero? (vector-ref (cdr grid) (third loc))))
                         (begin
                           (if (not (hash-table/get won-grid (first loc) #f))
                               (begin
                                 (hash-table/put! won-grid (first loc) #t)
                                 (yield (list (first loc) numbers))))))
                     (hash-table/put! grids (first loc) grid)
                     (update-grids (cdr ls))))
                 (play-numbers (cdr numbers))))))))))

;; play each number of the list numbers,
;; until one of the grid referenced in locations wins
(define (find-first-winning-grid locations numbers)
  (let ((iterate (winning-grids-iterator locations)))
    (iterate numbers)))

(assert (equal? '(2 (24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))
                (find-first-winning-grid (read-grids test-grids)
                                         (map string->number
                                              (string-split "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1" ",")))))

(define (find-score locations grid-id numbers-left)
  (let* ((winning-number (car numbers-left))
         (numbers (filter (lambda (num) (find (lambda (pos) (= (car pos) grid-id))
                                              (vector-ref locations num)))
                          (cdr numbers-left))))
    (* winning-number (apply + numbers))))

(assert (= 4512 (find-score (read-grids test-grids)
                            2 '(24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))))

  
;; part 1
(assert (= 29440 (let* ((lines (load-file "day_4_input.txt"))
                        (numbers (map string->number (string-split (car lines) ",")))
                        (locations (read-grids (cdr lines)))
                        (result (find-first-winning-grid locations numbers))
                        (winning-grid (first result))
                        (numbers-left (second result)))
                   (find-score locations winning-grid numbers-left))))


;; play each number of the list numbers,
;; until the last grid referenced in locations wins
(define (find-last-winning-grid locations numbers)
  (let* ((iterate (winning-grids-iterator locations))
         (all-grid-ids (fold (lambda (locs ids) (append ids (map first locs)))
                             '()
                             (vector->list locations)))
         (grid-count (length (uniq (sort all-grid-ids <)))))
    (let count-grids((count grid-count) (numbers-left numbers))
      (let ((result (iterate numbers-left)))
        (warn result)
        (if (= 1 count)
            result
            (count-grids (-1+ count)
                         (cdr (second result))))))))

(assert (equal? '(1 (13 6 15 25 12 22 18 20 8 19 3 26 1))
                (find-last-winning-grid (read-grids test-grids)
                                         (map string->number
                                              (string-split "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1" ",")))))


;; part 2
(let* ((lines (load-file "day_4_input.txt"))
       (numbers (map string->number (string-split (car lines) ",")))
       (locations (read-grids (cdr lines)))
       (result (find-last-winning-grid locations numbers))
       (winning-grid (first result))
       (numbers-left (second result)))
  (find-score locations winning-grid numbers-left))))
