#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(require srfi/1
         pyani-lib/vector
         pyani-lib/generic-ops
         "relch.ss"
         "test-functions.ss")

(define test-epsilon 1e-3)

(define-check (check-vectors= v1 v2 eps)
  (check-true (<= (p-vector-norm (- v1 v2)) eps)))

(define-check (vector-in-list? v list-of-vectors eps)
  (check-false (not (find (lambda (x) (<= (p-vector-norm (- v x)) eps))
                          list-of-vectors))))

;; Perform a check of given optimization method against a test problem
(define-check (check-optimization-method method problem max-iter eps parameter)
  (let ((result (method (test-problem-function problem)
                        (test-problem-x-start problem)
                        max-iter eps parameter
                        ;; Show simple progress bars
                        (lambda (x) (display ".") (flush-output))))
        (target-x (test-problem-target-x problem)))
    (if (list? target-x)
        (check-true (vector-in-list? result target-x test-epsilon))
        (check-vectors= result target-x test-epsilon))))

(define max-iterations 100)

(for-each
 (lambda (test-problem)
   (test-case
    (car test-problem)
    (after
     (check-optimization-method relch-optimize
                                (cdr test-problem)
                                (/ test-epsilon 100)
                                max-iterations 250)
     (newline))))
 test-problems)
