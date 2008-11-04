#lang scheme

;;; Wrappers for generic operations with matrices, vectors and numbers

;; Inspired by second chapter of SICP

(require "get-put.ss"
         (only-in "matrix.ss" matrix?)
         "matrix-ops.ss"
         "vector-ops.ss"
         "number-ops.ss"
         "mixed-ops.ss")

(provide + - * /)

;; `matrix?` has to be above everything else (that would not be needed
;; if I used tagged datums)
(define (type datum)
  (cond ((matrix? datum) 'matrix)
        ((vector? datum) 'vector)
        ((number? datum) 'number)))

(define (contents datum)
  datum)

;; I will not need coercions, so the most simple version of
;; `apply-generic` is used
(define (apply-generic op . args)
  (let* ((types (map type args))
         (proc (get op types)))
    (if proc
        (apply proc (map contents args))
        (error (format "OPERATION ~a IS NOT IMPLEMENTED FOR TYPES: ~a" op types)))))

;; TODO Try to rewrite this using `fold`
(define (nary-generic op)
  (define (generic v . rest)
    (if (null? rest)
        v
        (apply generic (append (list (apply-generic op v (first rest)))
                          (drop rest 1)))))
  generic)

(define + (nary-generic 'add))
(define * (nary-generic 'mul))
(define (reverse x) (apply-generic 'reverse x))

;; TODO Implement unary minus (opposite operation)
(define (- a b) (+ a (* b -1)))
(define (/ a b) (* a (reverse b)))

(install-vector-generics-package)
(install-matrix-generics-package)
(install-number-generics-package)
(install-mixed-generics-package)