#lang scheme

;;; Various functions for vectors

(require srfi/43)

(provide vector-sum
         absmax-vector-element p-vector-norm
         zero-vector)

;; Sum of all vector elements
(define (vector-sum vec)
  (vector-fold (lambda (i sum x) (+ sum x)) 0 vec))

(define (absmax-vector-element v)
  (vector-fold (lambda (i max e) (if (> (abs e) max) (abs e) max))
               (vector-ref v 0) v))

(define (p-vector-norm v [p 2])
  (expt (vector-sum
         (vector-map (lambda (i e) (expt (abs e) p)) v))
        (/ 1 p)))

(define (zero-vector n)
  (make-vector n 0))
