#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops
         pyani-lib/function-ops)

(provide relch-optimize
         relchf-optimize
         relcho-optimize
         gd-optimize)

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

;; Decrease shift factor until it leads to a better minimum with
;; foreseeing
(define (regulate-shift-foresee shift f x-start)
  (let ((x-new (+ x-start shift))
        (x-new-foresee (+ x-start (decrease-shift shift))))
    (if (or (not (better-minimum? f x-new x-start))
            (better-minimum? f x-new-foresee x-new))
        (regulate-shift-foresee (decrease-shift shift) f x-start)
        shift)))

;; Gradient descend
(define (gd-optimize f x-start
                     eps
                     iterations [unused #f]
                     [listen-proc #f])
  (define (choose-shift x-start G g)
    (regulate-shift (/ (* g -1) (p-vector-norm g)) f x-start))
  ((gradient-method choose-shift (lambda (f x) (zero-gradient? f x eps)))
   f x-start iterations listen-proc))

(define (regulate-shift-optimize shift f x-start)
  (let ((g (lambda (s) (@ f (+ x-start (* shift s))))))
    (* shift (vector-ref (gd-optimize g '#(1) 0.0001 100) 0))))

(define (make-relch-optimize regulate-shift)
  (lambda (f x-start
        eps
        iterations
        degree
        [listen-proc #f])
    (define (choose-shift x-start G g)
      (define (relch-shift L G g)
        (let ((n (matrix-size G)))
          (define (sub2 x) (sub1 (sub1 x)))
          ;; Calculate next degrees of shift given shifts for two
          ;; previous degrees
          (define (target-degree-shift times prev preprev)
            (let ((current-degree-shift
                   (+
                    (* (* (+ (identity-matrix n) (* G -2))
                          prev)
                       (/ (* 2 (sub1 L)) L))
                    (* preprev
                       (* -1 (/ (sub2 L) L)))
                    (* g (/ (* -4 (sub1 L)) L)))))
              (if (= times 1)
                  current-degree-shift
                  (target-degree-shift (sub1 times) current-degree-shift prev))))
          (let ((d1 (zero-vector n))
                (d2 (* g -2)))
            (cond ((= L 1) d1)
                  ((= L 2) d2)
                  (else (target-degree-shift (sub2 L) d2 d1))))))
      (let* ((G (normalize-matrix G))
             (g (normalize-vector g))
             (shift (relch-shift degree G g)))
        (regulate-shift shift f x-start)))
    ((gradient-method choose-shift (lambda (f x) (zero-gradient? f x eps)))
     f x-start iterations listen-proc)))

;; Different RELCH implementations
(define relch-optimize (make-relch-optimize regulate-shift))
(define relchf-optimize (make-relch-optimize regulate-shift-foresee))
(define relcho-optimize (make-relch-optimize regulate-shift-optimize))
