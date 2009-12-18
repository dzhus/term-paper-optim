#lang scheme

;;; Testsuite for RELCH implementation

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(require srfi/1
         (planet wmfarr/simple-matrix:1:0/matrix)
         pyani-lib/vector
         "relch.ss"
         "test-functions.ss")

(define test-epsilon 1e-3)

(define-check (check-vectors= v1 v2 eps)
  (check-true (<= (p-vector-norm (vector-sub v1 v2)) eps)))

(define-check (vector-in-list? v list-of-vectors eps)
  (check-false (not (find (lambda (x) (<= (p-vector-norm (vector-sub v x)) eps))
                          list-of-vectors))))

;; Perform a check of given optimization method against a test problem
(define-check (check-optimization-method method problem max-iter eps parameter)
  (display (format "~a: " (test-problem-name problem)))
  (flush-output)
  (let ((result (method (test-problem-function problem)
                        (test-problem-x-start problem)
                        max-iter eps parameter
                        ))
        (target-x (test-problem-target-x problem)))
    (if (list? target-x)
        (check-true (vector-in-list? result target-x test-epsilon))
        (check-vectors= result target-x test-epsilon))))

(define max-iterations 75)

(for-each
 (lambda (test-problem)
   (test-case
    (car test-problem)
    (after
     (check-optimization-method relch-optimize
                                (cdr test-problem)
                                max-iterations
                                (/ test-epsilon 1000)
                                250)
     (newline))))
 test-problems)
