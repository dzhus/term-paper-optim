#lang scheme

;;; Command line interface to optimization algorithms

;; This script provides a way to trace optimization process using
;; standard output stream. It may work with any function from
;; `test-functions` list defined in `test-functions.ss`.
;;
;; Usage:
;;
;;     mzscheme runner.ss -m relch -s -1,5.7 -i 100 -L 10 -p 3 rosenbrock
;;
;; Specifying empty string as an option value, e. g. `-i ""`, is the
;; same as specifying its default value. Typing `mzscheme runner.ss
;; --help` will give a brief help on command line options.

(require srfi/43
         srfi/48
         "test-functions.ss"
         "relch.ss")

(define (make-defaulting-parameter v)
  (make-parameter v (lambda (x) (if x x v))))

(define function-id (make-defaulting-parameter #f))
(define prec (make-defaulting-parameter 3))
(define iter (make-defaulting-parameter 100))
(define param (make-defaulting-parameter 8))
(define start-point (make-defaulting-parameter '#(0 0)))
(define method (make-defaulting-parameter relch-optimize))

;; We need this for `eval` to work in runtime
(define ons (module->namespace '"relch.ss"))

;; TODO: Use macros here
(command-line
 #:program "runner"
 #:once-each
 ;; Vulnerability follows :-)
 ["-m" m "Method (default relch)" (method
                                   (eval (string->symbol
                                          (string-append m "-optimize"))
                                         ons))]
 ["-s" s "Starting point (default 0.0,0.0)"
  (start-point (list->vector (map string->number (regexp-split #rx"," s))))]
 ["-i" i "Maximum iterations count (default 100)" (iter (string->number i))]
 ["-L" L "Chebyshev polynomial degree (default 8)" (param (string->number L))]
 ["-p" p "Precision (default 3)" (prec (string->number p))]
 #:args (f) (function-id f))

(define (point-poster x-start shift x-new g G)
  (vector-for-each
   (lambda (i p)
     (display (format (string-append "~0," (number->string (add1 (prec))) "F ") p)))
   x-start))

(let ((function (cdr (assoc (function-id) test-problems))))
  (exit ((method)
         (test-problem-function function)
         (start-point)
         (expt 10 (- (prec))) (iter) (param)
         point-poster)))
