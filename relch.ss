#lang scheme

;;; Multiparametric optimization with Chebyshev relaxation

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/generic-ops
         pyani-lib/function-ops)

;; Simple contracts for optimization parameters
(define point? vector?)
(define iteration-count? integer?)
(define epsilon? (and/c real? positive?))
;; Used only for \relch{}
(define parameter? (and/c integer? positive?))
  
;; Listener is a mean of providing insight to optimization process. If
;; listener returns a vector, it's considered to be a new minimum
;; point approximation
(define listener? (vector? vector? vector? vector? matrix? . -> . (or/c void vector?)))

;; All provided methods take five mandatory and one optional argument
;; and return an approximation to minimum point (represented by
;; vector)
(define optimization-method? (->* (procedure?
                                   vector?
                                   epsilon?
                                   iteration-count?
                                   parameter?)
                                  (listener?)
                                  point?))

(provide/contract [relch-optimize optimization-method?]
                  [gd-optimize optimization-method?]
                  [gdn-optimize optimization-method?])


(define (gradient-method choose-shift stop-condition)
  (define (optimize function x-start iterations [listener void])
    (if (<= iterations 0)
        x-start
        (let* ((g (@ (gradient function) x-start))
               (G (@ (hessian function) x-start))
               (shift (choose-shift x-start g G))
               (candidate-x-new (+ x-start shift))
               (listener-result (listener x-start
                                          shift
                                          candidate-x-new
                                          g G)))
          (let ((x-new (if (vector? listener-result)
                           listener-result
                           candidate-x-new)))
            (if (stop-condition function x-start x-new g G)
                x-new
                (optimize function x-new
                          (sub1 iterations)
                          listener))))))
  optimize)

(define (zero-gradient-condition eps)
  (lambda (f x-start x-new g G)
    (<= (p-vector-norm g) eps)))

(define (close-arguments-condition eps)
  (lambda (f x-start x-new g G)
    (<= (p-vector-norm (- x-start x-new)) eps)))

(define (decrease-shift shift)
  (/ shift 2))

(define (better-minimum? f x-new x)
  (< (@ f x-new) (@ f x)))


;; Shift regulations enforce relaxation condition enforcement for both
;; RELCH and GD, they also provide basic fallback measure in case of
;; infinite shifts

;; Decrease shift factor until it leads to a better value
(define (enforce-relaxation shift f x-start)
  (let ((x-new (+ x-start shift)))
    (if (better-minimum? f x-new x-start)
        shift
        (enforce-relaxation (decrease-shift shift) f x-start))))

;; Enforce relaxation only if it's possible; in case of numeric
;; overflow leading to infinite shift, return semirandom shift
(define (enforce-relaxation-filtered shift f x-start)
  (define (random-vector length)
    (build-vector length (lambda (x) (* 2 (- (random 2) 1/2)))))
  (if (< (p-vector-norm shift) +inf.0)
      (enforce-relaxation shift f x-start)
      (random-vector (vector-length shift))))


;; Gradient descend
(define (make-gd-optimize regulate-shift stop-condition)
  (lambda (f x-start
        eps
        iterations [unused #f]
        [listener void])
    (define (choose-shift x-start g G)
      (let ((shift (* g -0.1)))
        (regulate-shift shift f x-start)))
    ((gradient-method choose-shift (stop-condition eps))
     f x-start iterations listener)))

(define (make-relch-optimize regulate-shift stop-condition)
  (lambda (f x-start
        eps
        iterations
        degree
        [listener void])
    (define (choose-shift x-start g G)
      (define (relch-shift L g G)
        (let* ((n (matrix-size G))
               (d1 (zero-vector n))
               (d2 (* g -2)))
          (define (sub2 x) (sub1 (sub1 x)))
          ;; Calculate next degree of shift given shifts for two
          ;; previous degrees until degree reaches L
          (define (approach-target-degree [prev d2] [preprev d1] [degree 3])
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
                  (approach-target-degree current-degree-shift prev (add1 degree)))))
          (cond ((= L 1) d1)
                ((= L 2) d2)
                (else (approach-target-degree d2 d1)))))
      (let* ((G (normalize-matrix G))
             (g (normalize-vector g))
             (shift (relch-shift degree g G)))
        (regulate-shift shift f x-start)))
    ((gradient-method choose-shift (stop-condition eps))
     f x-start iterations listener)))

;; Different RELCH implementations
(define relch-optimize (make-relch-optimize enforce-relaxation-filtered zero-gradient-condition))

(define gd-optimize (make-gd-optimize enforce-relaxation zero-gradient-condition))

(define gdn-optimize (make-gd-optimize
                      (lambda (shift f x-start)
                        (enforce-relaxation (/ shift (p-vector-norm shift)) f x-start))
                      zero-gradient-condition))
