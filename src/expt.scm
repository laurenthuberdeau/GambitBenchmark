;;; EXPT -- A classic benchmark, computes a big exponent using bignums.

(define (expt-fun n)
  (expt 3 n))

(define (main . args)
  (run-benchmark
    "expt"
    fib-iters
    (lambda (result) #t)
    (lambda (n) (lambda () (expt-fun n)))
    10000000))
