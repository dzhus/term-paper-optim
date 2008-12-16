#lang scheme

;;; Gradient methods

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/vector-ops
         pyani-lib/function-ops)

;; Simple contracts for optimization parameters
(define function? procedure?)
(define point? vector?)
(define shift? vector?)
(define gradient? vector?)
(define hessian? matrix?)
(define iteration-count? integer?)
(define epsilon? (and/c real? positive?))
(define parameter? (and/c integer? positive?))
  
;; Listener is a sight from optimization process to outer world. When
;; listener returns a vector, it's considered to be a new minimum
;; point approximation; if listener returns a cons cell, then the
;; optimization stops with returned cell as a result. Listener may be
;; used to provide a trace of optimization process or check particular
;; conditions during optimization which may affect further
;; calculations.
;;
;; TODO Implement optimization methods in terms of streams, as what
;; listener actually does is processing the stream of consequent sets
;; of approximations
(define listener-reply? (or/c void point? cons?))
(define listener? (point? shift? point? gradient? matrix? . -> . listener-reply?))

;; All provided methods take five mandatory and one optional argument
;; and return an approximation to minimum point (represented by
;; vector) or listener's reply
(define optimization-method? (->* (procedure?
                                   point?
                                   iteration-count?)
                                  (listener?)
                                  (or/c point? listener-reply?)))

;; Algorithm used to choose shift on every iteration is the essential
;; difference between all gradient methods
(define shift-chooser? (point? vector? matrix? . -> . vector?))

;; Used to stop optimization process when some particular conditions
;; are met. Not to be confused with maximum iteration count, which is
;; semantically different and should not be handled here.
(define stop-condition? (function? point? point? gradient? hessian? . -> . boolean?))

(provide/contract [gradient-method
                   (shift-chooser? stop-condition? . -> . optimization-method?)]
                  [zero-gradient-condition
                   (epsilon? . -> . stop-condition?)]
                  [close-arguments-condition
                   (epsilon? . -> . stop-condition?)]
                  [optimization-method? contract?])


(define (gradient-method choose-shift stop-condition)
  (define (optimize function x-start iterations [listener void])
    (if (<= iterations 0)
        x-start
        (let* ((g (@ (gradient function) x-start))
               (G (@ (hessian function) x-start))
               (shift (choose-shift x-start g G))
               (candidate-x-new (vector-+-vector x-start shift))
               (listener-result (listener x-start
                                          shift
                                          candidate-x-new
                                          g G)))
          (if (cons? listener-result)
              listener-result
              (let ((x-new (if (vector? listener-result)
                               listener-result
                               candidate-x-new)))
                (if (stop-condition function x-start x-new g G)
                    x-new
                    (optimize function x-new
                              (sub1 iterations)
                              listener)))))))
  optimize)


;; Standard conditions
(define (zero-gradient-condition eps)
  (lambda (f x-start x-new g G)
    (<= (p-vector-norm g) eps)))

(define (close-arguments-condition eps)
  (lambda (f x-start x-new g G)
    (<= (p-vector-norm (- x-start x-new)) eps)))
