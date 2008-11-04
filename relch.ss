#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         "matrix.ss"
         "vectors.ss")

(define (shift L G g)
  (define (sub2 x) (sub1 (sub1 x)))
  (let ((n (matrix-size G)))
    (cond ((= L 1) (zero-vector n))
          ((= L 2) (vector-*-number g -2))
          (else (vectors-add
                 (vector-*-number
                  (matrix-*-vector
                   (matrix-+-matrix
                    (identity-matrix n)
                    (matrix-*-number G -2))
                   (shift (sub1 L) G g))
                  (/ (* 2 (sub1 L)) L))
                 (vector-*-number
                  (shift (sub2 L) G g)
                  (- (/ (sub2 L) L)))
                 (vector-*-number
                  g
                  (- (/ (* 4 (sub1 L)) L))))))))

(define (optimize f x [iterations 20] [L 20])
  (define @ at-vector)
  (define (better-solution? x-new)
    (< (@ f x-new) (@ f x)))
  (define (choose-shift-factor shift [alpha 1])
    (let ((x-new (vectors-add x (vector-*-number shift alpha))))
      (if (better-solution? x-new)
          alpha
          (choose-shift-factor shift (/ alpha 2)))))
  (if (<= iterations 1)
      x
      (let* ((g (normalize-vector (@ (gradient f) x)))
             (G (normalize-matrix (@ (hessian f) x)))
             (shift (shift L G g))
             (alpha (choose-shift-factor shift)))
        (let ((x-new (vectors-add x (vector-*-number shift alpha))))
          (optimize f x-new (sub1 iterations))))))