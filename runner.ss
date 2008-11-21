#lang scheme

;;; Command line interface to optimization algorithms

;; This script provides a way to trace optimization process using
;; standard output stream. It may work with any function from
;; `test-functions` list defined in `test-functions.ss`.
;;
;; Usage:
;;
;;     mzscheme runner.ss -s -1,5.7 -p 3 -i 100 -L 10 rosenbrock

(require srfi/43
         srfi/48
         "test-functions.ss"
         "relch.ss")

(define (point-poster point [nl #t])
  (vector-for-each
   (lambda (i p)
     (display (format "~0,6F " p)))
   point)
  (when nl (display "\n")))

(define function-id (make-parameter #f))
(define prec (make-parameter 3))
(define iter (make-parameter 100))
(define deg (make-parameter 8))
(define start-point (make-parameter '#(0 0)))

(command-line
 #:program "runner"
 #:once-each
 ["-i" i "Maximum iterations count (default 100)" (iter (string->number i))]
 ["-p" p "Precision (default 3)" (prec (string->number p))]
 ["-L" L "Chebyshev polynomial degree (default 8)" (deg (string->number L))]
 ["-s" s "Starting point (default 0.0,0.0)"
  (start-point (list->vector (map string->number (regexp-split #rx"," s))))]
 #:args (f) (function-id f))

(let ((function (cdr (assoc (function-id) test-functions))))
  (exit (relch-optimize (test-function-def function)
                        (start-point)
                        #:eps (expt 10 (- (prec)))
                        #:iterations (iter)
                        #:degree (deg)
                        #:listener point-poster)))
