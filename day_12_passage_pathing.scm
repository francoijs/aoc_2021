;; https://adventofcode.com/2021/day/12

(load "utils.scm")

(define (read-cave-graph lines)
  (fold (lambda (line graph)
          (let* ((caves (string-split line "-"))
                 (neighbors-start (hash-table/get graph (first caves) #f))
                 (neighbors-end (hash-table/get graph (second caves) #f)))
            (hash-table/put! graph (first caves)
                             (if neighbors-start
                                 (cons (second caves) neighbors-start)
                                 (list (second caves))))
            (hash-table/put! graph (second caves)
                             (if neighbors-end
                                 (cons (first caves) neighbors-end)
                                 (list (first caves))))
            graph))
        (make-string-hash-table)
        lines))
(define *test-graph-1* (read-cave-graph '("start-A"
                                          "start-b"
                                          "A-c"
                                          "A-b"
                                          "b-d"
                                          "A-end"
                                          "b-end")))
(assert (equal? '("b" "A") (hash-table/get *test-graph-1* "start" #f)))
(assert (equal? '("A") (hash-table/get *test-graph-1* "c" #f))))

(define (big-cave? name)
  (char-upper-case? (car (string->list name))))
(define small-cave? (compose not big-cave?))
(assert (big-cave? "A"))
(assert (big-cave? "HN"))
(assert (not (big-cave? "c")))
(assert (not (small-cave? "A")))
(assert (not (small-cave? "HN")))
(assert (small-cave? "c"))

(define (find-paths graph #!optional twice-visitable)
  (let ((twice-visitable (if (default-object? twice-visitable)
                             "none"
                             twice-visitable)))                             
    (let follow ((node "start")
                 (already-visited (make-string-hash-table))
                 (twice-visitable-count 0))
      (if (string=? node "end")
          '("end")
          (fold (lambda (neighbor paths)
                  ;;                (warn node neighbor)
                  (if (and (small-cave? neighbor)
                           (hash-table/get already-visited neighbor #f))
                      paths
                      (let ((twice-visitable-count
                             (+ twice-visitable-count
                                (if (string=? node twice-visitable) 1 0))))
                        (if (or (and (small-cave? node)
                                     (not (string=? node twice-visitable)))
                                (and (string=? node twice-visitable)
                                     (> twice-visitable-count 1)))
                            (hash-table/put! already-visited node #t))
                        (append paths
                                (map (lambda (path) (string-append node "-" path))
                                     (follow neighbor
                                             (string-hash-table/copy already-visited)
                                             twice-visitable-count))))))
                '()
                (hash-table/get graph node #f))))))
(assert (= 10 (length (find-paths *test-graph-1*))))

(define *test-graph-2* (read-cave-graph '("fs-end"
                                          "he-DX"
                                          "fs-he"
                                          "start-DX"
                                          "pj-DX"
                                          "end-zg"
                                          "zg-sl"
                                          "zg-pj"
                                          "pj-he"
                                          "RW-he"
                                          "fs-DX"
                                          "pj-RW"
                                          "zg-RW"
                                          "start-pj"
                                          "he-WI"
                                          "zg-he"
                                          "pj-fs"
                                          "start-RW")))
(assert (= 226 (length (find-paths *test-graph-2*))))

;; part 1
(assert (= 4411 (chain "day_12_input.txt"
                       load-file
                       read-cave-graph
                       find-paths
                       length)))


(define (find-small-caves graph)
  (filter (lambda (cave) (and (small-cave? cave)
                              (not (string=? cave "start"))
                              (not (string=? cave "end"))))
          (hash-table/key-list graph)))
(assert (= 3 (length (find-small-caves *test-graph-1*))))
(assert (= 5 (length (find-small-caves *test-graph-2*))))

(define (count-paths-with-twice-visitable graph)
  (length (uniq (sort (fold (lambda (cave paths)
                              (append paths (find-paths graph cave)))
                            '()
                            (find-small-caves graph))
                      string<?))))
(assert (= 36 (count-paths-with-twice-visitable *test-graph-1*)))
(assert (= 3509 (count-paths-with-twice-visitable *test-graph-2*)))

;; part 2
(assert (= 136767 (chain "day_12_input.txt"
                         load-file
                         read-cave-graph
                         count-paths-with-twice-visitable)))
