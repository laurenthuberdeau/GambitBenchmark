;;;----------------------------------------------------------------------------

(define (stats vals)
  (let* ((nb-bins 20)
         (nb-vals (length vals))
         (min-val (apply min vals))
         (max-val (apply max vals))
         (mean (/ (apply + vals) nb-vals))
         (sd (sqrt (/ (apply + (map (lambda (v) (let ((d (- v mean))) (* d d))) vals)) nb-vals)))
         (diff (- max-val min-val))
         (step (/ diff (- nb-bins 1)))
         (start (- min-val (* 0.5 step)))
         (v (make-vector nb-bins '())))

    (define decimals 6)

    (define (num->string num w d)      ; w = total width, d = decimals
      (let ((n (floor (inexact->exact (round (* (abs num) (expt 10 d)))))))
        (let ((i (quotient n (expt 10 d)))
              (f (modulo n (expt 10 d))))
          (let ((si (string-append
                     (if (< num 0) "-" "")
                     (if (and (= i 0) (> d 0)) "" (number->string i 10))))
                (sf (number->string (+ f (expt 10 d)) 10)))
            (if (> d 0)
                (string-set! sf 0 #\.)
                (set! sf ""))
            (let ((lsi (string-length si))
                  (lsf (string-length sf)))
              (let ((blanks (- w (+ lsi lsf))))
                (string-append (make-string (max blanks 0) #\space) si sf)))))))

    (let ((index 0))
      (for-each
       (lambda (val)
         (let ((bin (inexact->exact (floor (/ (- val start) step)))))
           (vector-set! v bin (cons index (vector-ref v bin)))
           (set! index (+ index 1))))
       vals))

    (let loop ((x min-val) (i 0))
      (if (< i nb-bins)
          (begin
            (display
             (string-append
              (num->string x (+ decimals 3) decimals)
              " ("
              (num->string (* 100 (/ x mean)) 6 2)
              "%):"
              (num->string (* 100 (/ (length (vector-ref v i)) nb-vals)) 3 0)
              "% "
              (list->string
               (map (lambda (index)
                      (integer->char (+ (char->integer #\A)
                                        (inexact->exact (floor (/ (* index 26) nb-vals))))))
                    (reverse (vector-ref v i))))
              (if (and (>  mean (+ x (* -0.5 step)))
                       (<= mean (+ x (* +0.5 step))))
                  (string-append "      <--- mean="
                                 (num->string mean 0 decimals))
                  "")
              "\n"))
            (loop (+ x step) (+ i 1)))))

    (display
     (string-append
      (num->string sd (+ decimals 3) decimals)
      " ("
      (num->string (* 100 (/ sd mean)) 6 4)
      "%) standard deviation\n"))))

;; List and String utils

(define (list-split lst pred)
  (define (worker lst accum)
    (if (null? lst)
      ;; Dont add accum if empty
      (if (not (null? accum))
        (list (reverse accum))
        '())
      (let ((head (car lst))
            (tail (cdr lst)))
        (if (pred head)
          ;; Dont add accum if empty
          (if (not (null? accum))
            (cons (reverse accum) (worker tail '()))
            (worker tail '()))
          (worker tail (cons head accum))))))
  (worker lst '()))

(define (string-split str pred)
  (let ((pred
          (cond
            ((procedure? pred) pred)
            ((list? pred) (lambda (c) (not (= -1 (index-of c pred)))))
            ((string? pred) (lambda (c) (not (= -1 (index-of c (string->list pred))))))
            ((char? pred) (lambda (c) (not (eqv? c pred))))
            (else
              (display "Unknown predicate:" pred)
              (exit)))))
    (map list->string (list-split (string->list str) pred))))

(define (find pred elems #!optional (index 0))
  (if (null? elems)
    -1
    (if (pred (car elems))
      index
      (find pred (cdr elems) (+ 1 index)))))

(define (index-of elem elems)
  (find (lambda (var) (equal? var elem)) elems))

;; IO Utils

(define (read-lines)
  (define (worker accum)
    (let ((line (read-line)))
      (if (eof-object? line)
        (reverse accum)
        (worker (cons line accum)))))
  (worker '()))

(define (read-csv)
  (let ((lines (read-lines)))
      (map (lambda (str) (string-split str ",")) lines)))

;; CSV Fields definitions

(define (csv-number lst index) (string->number (list-ref lst index)))

(define (sample-time lst)        (csv-number lst 0))
(define (temp-core-0-before lst) (csv-number lst 1))
(define (temp-core-1-before lst) (csv-number lst 2))
(define (temp-core-2-before lst) (csv-number lst 3))
(define (temp-core-3-before lst) (csv-number lst 4))
(define (temp-core-0-after  lst) (csv-number lst 5))
(define (temp-core-1-after  lst) (csv-number lst 6))
(define (temp-core-2-after  lst) (csv-number lst 7))
(define (temp-core-3-after  lst) (csv-number lst 8))

(define (freq-core-0-before lst) (csv-number lst 9))
(define (freq-core-1-before lst) (csv-number lst 10))
(define (freq-core-2-before lst) (csv-number lst 11))
(define (freq-core-3-before lst) (csv-number lst 12))
(define (freq-core-0-after  lst) (csv-number lst 13))
(define (freq-core-1-after  lst) (csv-number lst 14))
(define (freq-core-2-after  lst) (csv-number lst 15))
(define (freq-core-3-after  lst) (csv-number lst 16))

;; CSV Computed fields

(define (max-temp-before lst) (max
  (temp-core-0-before lst)
  (temp-core-1-before lst)
  (temp-core-2-before lst)
  (temp-core-3-before lst)))
(define (max-temp-after lst) (max
  (temp-core-0-after lst)
  (temp-core-1-after lst)
  (temp-core-2-after lst)
  (temp-core-3-after lst)))

(define (max-freq-before lst) (max
  (freq-core-0-before lst)
  (freq-core-1-before lst)
  (freq-core-2-before lst)
  (freq-core-3-before lst)))
(define (max-freq-after lst) (max
  (freq-core-0-after lst)
  (freq-core-1-after lst)
  (freq-core-2-after lst)
  (freq-core-3-after lst)))

(define (delta-temp  lst) (abs (- (max-temp-before lst) (max-temp-after lst))))
(define (delta-freq  lst) (abs (- (max-freq-before lst) (max-freq-after lst))))

;; Entry point

(define (main args)
  (let ((results (read-csv)))
    (stats (map sample-time results))))

(main (cdr (command-line)))
