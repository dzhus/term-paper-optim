#lang scheme

(require srfi/43
         "vectors.ss")

(provide matrix row column
         matrix-row row-item column-item matrix-item
         matrix-size row-length column-length
         matrix-rows matrix-columns
         matrix-map
         build-matrix
         matrix-*-vector matrix-+-matrix)

(define matrix vector)
(define row vector)
(define column vector)

(define matrix-row vector-ref)
(define row-item vector-ref)
(define column-item vector-ref)
(define (matrix-item matrix i j)
  (row-item (matrix-row matrix i) j))

(define row-length vector-length)
(define column-length vector-length)

(define matrix-size vector-length)
(define matrix-rows vector-length)
(define (matrix-columns m)
  (row-length (matrix-row m 0)))

(define (matrix-map proc matrix)
  (vector-map (lambda (i row)
                (vector-map
                 (lambda (j item)
                   (proc i j item))
                 row))
              matrix))

(define (rows-map proc matrix)
  (vector-map
   (lambda (row-index row)
     (proc row-index row))
   matrix))

(define (build-matrix proc rows columns)
  (matrix-map (lambda (i j e) (proc i j))
              (make-vector rows (make-vector columns))))

(define (identity-matrix n)
  (define (kronecker i j) (if (= i j) 1 0))
  (build-matrix kronecker n n))

(define (matrix-*-vector m v)
  (build-vector (matrix-size m)
                (lambda (i) (vector-sum
                        (vector-map (lambda (i e1 e2) (* e1 e2))
                                    (matrix-row m i)
                                    v)))))

(define (matrix-+-matrix m1 m2)
  (let ((m (matrix-rows m1))
        (n (matrix-columns m1)))
  (build-matrix (lambda (i j)
                  (+ (matrix-item m1 i j)
                     (matrix-item m2 i j)))
                m n)))

(define (matrix-*-number m x)
  (matrix-map (lambda (i j e) (* e x)) m))

(define (matrix-/-number m x)
  (matrix-*-number m (/ 1 x)))

(define (absmax-matrix-element m)
  (vector-norm
   (rows-map
    (lambda (i row) (vector-norm row)) m)))

(define (matrix-norm m)
  (absmax-matrix-element m))

(define (normalize-matrix m)
  (matrix-/-number m (matrix-norm m)))