#lang scheme

;;; Gradient methods

(require "shared.ss"
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/vector-ops
         pyani-lib/function-ops)

;; Simple contracts for optimization parameters
(define function? (flat-contract procedure?))
(define point? (flat-contract vector?))
(define shift? (flat-contract vector?))
(define gradient? (flat-contract vector?))
(define hessian? (flat-contract matrix?))
(define iteration-count? (flat-contract integer?))
(define epsilon? (and/c real? positive?))
  
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
(define gradient-method?
  (->* (procedure? point? iteration-count?)
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
                   (shift-chooser? stop-condition? . -> . gradient-method?)]
                  [zero-gradient-condition
                   (epsilon? . -> . stop-condition?)])

(provide/contract [function? contract?]
                  [point? contract?]
                  [iteration-count? contract?]
                  [epsilon? contract?]
                  [listener? contract?]
                  [listener-reply? contract?])

;;@ $x^{k+1} = x^k - H_k\left(G_k, h_k\right) g_k$
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

;;@ $\norm{f'(x)} < \epsilon$
(define (zero-gradient-condition eps)
  (lambda (f x-start x-new g G)
    (<= (p-vector-norm g) eps)))
