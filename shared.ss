#lang scheme

(require srfi/43
         pyani-lib/vector
         pyani-lib/matrix
         pyani-lib/generic-ops)

(provide normalize-vector
         normalize-matrix
         max-slice)

(define (normalize-vector v)
  (/ v (p-vector-norm v)))

(define (normalize-matrix m)
  (/ m (euclidean-matrix-norm m)))

(define (max-slice g)
  (lambda (h . x) (max 0 (apply g (append (list h) x)))))
