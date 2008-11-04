#lang scheme

;;; Matrix package

;; Matrices are implemented as vectors of vectors. Many of these
;; functions are specific to such representation.

(require srfi/43
         "vector.ss")

(provide matrix row column matrix?
         matrix-row row-item column-item matrix-item
         matrix-size row-length column-length
         matrix-rows matrix-columns
         matrix-map
         build-matrix identity-matrix
         euclidean-norm)

;; Matrix, row and column constructors
(define matrix vector)
(define row vector)
(define column vector)

;; Predicate
;; 
;; TODO Extend the definition
(define (matrix? m)
  (and (vector? m)
       (vector-every vector? m)))

;; Selectors
(define (matrix-item matrix i j)
  (row-item (matrix-row matrix i) j))
(define matrix-row vector-ref)
(define matrix-column vector-ref)

(define row-item vector-ref)
(define column-item vector-ref)

(define row-length vector-length)
(define column-length vector-length)

;; Square matrix only
(define matrix-size vector-length)

(define matrix-rows vector-length)
(define (matrix-columns m)
  (row-length (matrix-row m 0)))

;; List-style maps for matrices
(define (rows-map proc matrix)
  (vector-map (lambda (row-index row) (proc row)) matrix))
(define (row-map proc row)
  (vector-map (lambda (i e) (proc e)) row))
(define (column-map proc column)
  (vector-map (lambda (i e) (proc e)) column))

(define (matrix-map proc matrix)
  (vector-map
   (lambda (i row)
     (vector-map
      (lambda (j item)
        (proc i j item))
      row))
   matrix))

;; Generic matrix constructor
(define (build-matrix proc rows columns)
  (matrix-map (lambda (i j e) (proc i j))
              (make-vector rows (make-vector columns))))

;; Norms
(define (euclidean-norm m)
  (sqrt
   (vector-sum
    (rows-map
     (lambda (row)
       (vector-sum
        (row-map (lambda (e) (sqr e)) row)))
     m))))

;; Special matrices
(define (identity-matrix n)
  (define (kronecker i j) (if (= i j) 1 0))
  (build-matrix kronecker n n))