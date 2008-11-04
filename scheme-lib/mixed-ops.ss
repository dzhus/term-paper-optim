#lang scheme

;;; Mixed-type operations for matrices, vectors and numbers

(require srfi/43
         "matrix.ss"
         "vector.ss"
         "get-put.ss")

(provide matrix-*-vector
         matrix-*-number
         vector-*-number
         install-mixed-generics-package)

(define (matrix-*-vector m v)
  (build-vector (matrix-size m)
                (lambda (i) (vector-sum
                        (vector-map (lambda (i e1 e2) (* e1 e2))
                                    (matrix-row m i)
                                    v)))))

(define (matrix-*-number m x)
  (matrix-map (lambda (i j e) (* e x)) m))

(define (vector-*-number v s)
  (vector-map (lambda (i x) (* x s)) v))

(define (install-mixed-generics-package)
  (put 'mul '(vector number) vector-*-number)
  (put 'mul '(matrix vector) matrix-*-vector)
  (put 'mul '(matrix number) matrix-*-number))