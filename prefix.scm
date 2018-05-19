
;; The following 2 variables are defined
;; sample-count: Number of time to execute and record bench
;; warm-run-count: Number of time to execute bench before recording time

(define (run-benchmark name count ok? run-maker . args)
  (let* ((run (apply run-maker args)))
    (if warm-run-count (go run warm-run-count #f))
    (go run sample-count #t)))

(define (go fun n show)
 (let loop ((n n))
   (if (> n 0)
	  (begin
      ;; Getting fresh heap
      (##gc)

      ;; Measuring
  	  (let* ((temps-before (get-cpu-temps))
             (freqs-before (get-cpu-freqs))
             (stats (##exec-stats fun))
  		       (r (cdr (assoc 'real-time stats))))
  	    (if show
          (begin
  	       (print r)
           (print ",")
           ;; The following prints contains ',' at their end.
           ;; No need to print ','
           (print temps-before)
           (print (get-cpu-temps))
           (print freqs-before)
           (print (get-cpu-freqs))
           (print "\n")
           )))

	  (loop (- n 1))))))

(define (execute-shell-command cmd)
  (let ((result (shell-command cmd #t)))
    (if (= 0 (car result))
      (cdr result)
      #f)))

(define (get-cpu-temps)
  (execute-shell-command "sensors | grep Core | awk '{print $3}' | tr -d '+°C' | tr '\n' ','"))

(define (get-cpu-freqs)
  (execute-shell-command "cat /proc/cpuinfo | grep '^cpu MHz' | awk '{print $4}' | tr '\n' ','"))

(define (fatal-error . args)
  (for-each display args)
  (newline)
  (exit 1))

(define (call-with-output-file/truncate filename proc)
  (call-with-output-file filename proc))

(define-macro (if-fixflo yes no) no)

;------------------------------------------------------------------------------

; Macros...

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

(if-fixflo

(begin

; Specialize fixnum and flonum arithmetic.

;; This code should be used when f64vectors are available.
;(def-macro (FLOATvector-const . lst)   `',(list->f64vector lst))
;(def-macro (FLOATvector? x)            `(f64vector? ,x))
;(def-macro (FLOATvector . lst)         `(f64vector ,@lst))
;(def-macro (FLOATmake-vector n . init) `(make-f64vector ,n ,@init))
;(def-macro (FLOATvector-ref v i)       `(f64vector-ref ,v ,i))
;(def-macro (FLOATvector-set! v i x)    `(f64vector-set! ,v ,i ,x))
;(def-macro (FLOATvector-length v)      `(f64vector-length ,v))
;
;(def-macro (nuc-const . lst)
;  `',(list->vector
;       (map (lambda (x)
;              (if (vector? x)
;                (list->f64vector (vector->list x))
;                x))
;            lst)))

(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
(def-macro (FLOATvector? x)            `(vector? ,x))
(def-macro (FLOATvector . lst)         `(vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(make-vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector lst))

(def-macro (FLOAT+ . lst) `(fl+ ,@lst))
(def-macro (FLOAT- . lst) `(fl- ,@lst))
(def-macro (FLOAT* . lst) `(fl* ,@lst))
(def-macro (FLOAT/ . lst) `(fl/ ,@lst))
(def-macro (FLOAT= . lst)  `(fl= ,@lst))
(def-macro (FLOAT< . lst)  `(fl< ,@lst))
(def-macro (FLOAT<= . lst) `(fl<= ,@lst))
(def-macro (FLOAT> . lst)  `(fl> ,@lst))
(def-macro (FLOAT>= . lst) `(fl>= ,@lst))
(def-macro (FLOATnegative? . lst) `(flnegative? ,@lst))
(def-macro (FLOATpositive? . lst) `(flpositive? ,@lst))
(def-macro (FLOATzero? . lst)     `(flzero? ,@lst))
(def-macro (FLOATabs . lst) `(flabs ,@lst))
(def-macro (FLOATsin . lst) `(flsin ,@lst))
(def-macro (FLOATcos . lst) `(flcos ,@lst))
(def-macro (FLOATatan . lst) `(flatan ,@lst))
(def-macro (FLOATsqrt . lst) `(flsqrt ,@lst))
(def-macro (FLOATmin . lst) `(flmin ,@lst))
(def-macro (FLOATmax . lst) `(flmax ,@lst))
(def-macro (FLOATround . lst) `(flround ,@lst))
(def-macro (FLOATinexact->exact . lst) `(inexact->exact ,@lst))

(define (GENERIC+ x y) (+ x y))
(define (GENERIC- x y) (- x y))
(define (GENERIC* x y) (* x y))
(define (GENERIC/ x y) (/ x y))
(define (GENERICquotient x y) (quotient x y))
(define (GENERICremainder x y) (remainder x y))
(define (GENERICmodulo x y) (modulo x y))
(define (GENERIC= x y) (= x y))
(define (GENERIC< x y) (< x y))
(define (GENERIC<= x y) (<= x y))
(define (GENERIC> x y) (> x y))
(define (GENERIC>= x y) (>= x y))
(define (GENERICexpt x y) (expt x y))

(def-macro (+ . lst) `(fxwrap+ ,@lst))
(def-macro (- . lst) `(fxwrap- ,@lst))
(def-macro (* . lst) `(fxwrap* ,@lst))
(def-macro (quotient . lst) `(fxwrapquotient ,@lst))
(def-macro (modulo . lst) `(fxmodulo ,@lst))
(def-macro (remainder . lst) `(fxremainder ,@lst))
(def-macro (= . lst)  `(fx= ,@lst))
(def-macro (< . lst)  `(fx< ,@lst))
(def-macro (<= . lst) `(fx<= ,@lst))
(def-macro (> . lst)  `(fx> ,@lst))
(def-macro (>= . lst) `(fx>= ,@lst))
(def-macro (negative? . lst) `(fxnegative? ,@lst))
(def-macro (positive? . lst) `(fxpositive? ,@lst))
(def-macro (zero? . lst) `(fxzero? ,@lst))
(def-macro (odd? . lst) `(fxodd? ,@lst))
(def-macro (even? . lst) `(fxeven? ,@lst))
(def-macro (bitwise-or . lst) `(fxior ,@lst))
(def-macro (bitwise-and . lst) `(fxand ,@lst))
(def-macro (bitwise-not . lst) `(fxnot ,@lst))
)

(begin

; Don't specialize fixnum and flonum arithmetic.

(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
(def-macro (FLOATvector? x)            `(vector? ,x))
(def-macro (FLOATvector . lst)         `(vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(make-vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector lst))

(def-macro (FLOAT+ . lst) `(+ ,@lst))
(def-macro (FLOAT- . lst) `(- ,@lst))
(def-macro (FLOAT* . lst) `(* ,@lst))
(def-macro (FLOAT/ . lst) `(/ ,@lst))
(def-macro (FLOAT= . lst)  `(= ,@lst))
(def-macro (FLOAT< . lst)  `(< ,@lst))
(def-macro (FLOAT<= . lst) `(<= ,@lst))
(def-macro (FLOAT> . lst)  `(> ,@lst))
(def-macro (FLOAT>= . lst) `(>= ,@lst))
(def-macro (FLOATnegative? . lst) `(negative? ,@lst))
(def-macro (FLOATpositive? . lst) `(positive? ,@lst))
(def-macro (FLOATzero? . lst)     `(zero? ,@lst))
(def-macro (FLOATabs . lst) `(abs ,@lst))
(def-macro (FLOATsin . lst) `(sin ,@lst))
(def-macro (FLOATcos . lst) `(cos ,@lst))
(def-macro (FLOATatan . lst) `(atan ,@lst))
(def-macro (FLOATsqrt . lst) `(sqrt ,@lst))
(def-macro (FLOATmin . lst) `(min ,@lst))
(def-macro (FLOATmax . lst) `(max ,@lst))
(def-macro (FLOATround . lst) `(round ,@lst))
(def-macro (FLOATinexact->exact . lst) `(inexact->exact ,@lst))

(def-macro (GENERIC+ . lst) `(+ ,@lst))
(def-macro (GENERIC- . lst) `(- ,@lst))
(def-macro (GENERIC* . lst) `(* ,@lst))
(def-macro (GENERIC/ . lst) `(/ ,@lst))
(def-macro (GENERICquotient . lst)  `(quotient ,@lst))
(def-macro (GENERICremainder . lst) `(remainder ,@lst))
(def-macro (GENERICmodulo . lst)    `(modulo ,@lst))
(def-macro (GENERIC= . lst)  `(= ,@lst))
(def-macro (GENERIC< . lst)  `(< ,@lst))
(def-macro (GENERIC<= . lst) `(<= ,@lst))
(def-macro (GENERIC> . lst)  `(> ,@lst))
(def-macro (GENERIC>= . lst) `(>= ,@lst))
(def-macro (GENERICexpt . lst) `(expt ,@lst))
)
)

;------------------------------------------------------------------------------
