#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops
         pyani-lib/function-ops)

(provide relch-optimize
         gd-optimize
         gdn-optimize)

(define (gradient-method choose-shift stop-condition)
  (define (optimize function x-start iterations [listen-proc #f])
    (when listen-proc (listen-proc x-start))
    (if (or (<= iterations 1)
            (stop-condition function x-start))
        x-start
        (let* ((G (@ (hessian function) x-start))
               (g (@ (gradient function) x-start))
               (shift (choose-shift x-start G g))
               (x-new (+ x-start shift)))
          (optimize function x-new
                    (sub1 iterations)
                    listen-proc))))
  optimize)

(define (zero-gradient? f x eps)
  (<= (p-vector-norm (@ (gradient f) x)) eps))

(define (decrease-shift shift)
  (/ shift 2))

(define (better-minimum? f x-new x)
  (< (@ f x-new) (@ f x)))

;; Decrease shift factor until it leads to a better minimum
(define (regulate-shift shift f x-start)
  (let ((x-new (+ x-start shift)))
    (if (better-minimum? f x-new x-start)
        shift
        (regulate-shift (decrease-shift shift) f x-start))))

;; Gradient descend
(define (make-gd-optimize regulate-shift)
  (lambda (f x-start
        eps
        iterations [unused #f]
        [listen-proc #f])
    (define (choose-shift x-start G g)
      (regulate-shift (* g -1) f x-start))
  ((gradient-method choose-shift (lambda (f x) (zero-gradient? f x eps)))
   f x-start iterations listen-proc)))

(define (make-relch-optimize regulate-shift)
  (lambda (f x-start
        eps
        iterations
        degree
        [listen-proc #f])
    (define (choose-shift x-start G g)
      (define (relch-shift L G g)
        (let* ((n (matrix-size G))
               (d1 (zero-vector n))
               (d2 (* g -2)))
          (define (sub2 x) (sub1 (sub1 x)))
          ;; Calculate next degree of shift given shifts for two
          ;; previous degrees until degree reaches L
          (define (next-degree-shift [prev d2] [preprev d1] [degree 3])
            (let ((current-degree-shift
                   (+
                    (* (* (+ (identity-matrix n) (* G -2))
                          prev)
                       (/ (* 2 (sub1 degree)) degree))
                    (* preprev
                       (* -1 (/ (sub2 degree) degree)))
                    (* g (/ (* -4 (sub1 degree)) degree)))))
              (if (= degree L)
                  current-degree-shift
                  (next-degree-shift current-degree-shift prev (add1 degree)))))
          (cond ((= L 1) d1)
                ((= L 2) d2)
                (else (next-degree-shift d2 d1)))))
      (let* ((G (normalize-matrix G))
             (g (normalize-vector g))
             (shift (relch-shift degree G g)))
        (regulate-shift shift f x-start)))
    ((gradient-method choose-shift (lambda (f x) (zero-gradient? f x eps)))
     f x-start iterations listen-proc)))

;; Different RELCH implementations
(define relch-optimize (make-relch-optimize regulate-shift))
(define gd-optimize (make-gd-optimize regulate-shift))
(define gdn-optimize (make-gd-optimize
                      (lambda (shift f x-start)
                        (regulate-shift (/ shift (p-vector-norm shift)) f x-start))))
