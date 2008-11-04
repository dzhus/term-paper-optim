#lang scheme

(require srfi/43)

(provide vector-sum
         vectors-add vector-*-number vector-/-number
         vector-norm normalize-vector
         zero-vector)

(define (vector-sum vec)
  (vector-fold (lambda (i sum x) (+ sum x)) 0 vec))

(define (vectors-add v . rest)
  (if (null? rest)
      v
      (apply vectors-add (append (list (vector-map (lambda (i x y) (+ x y))
                                                   v (first rest)))
                                 (drop rest 1)))))

(define (vector-*-number v s)
  (vector-map (lambda (i x) (* x s)) v))

(define (vector-/-number v s)
  (vector-*-number v (/ 1 s)))

(define (absmax-vector-element v)
  (vector-fold (lambda (i max e) (if (> (abs e) max) (abs e) max))
               (vector-ref v 0) v))

(define (p-vector-norm v [p 2])
  (expt (vector-sum
         (vector-map (lambda (i e) (expt (abs e) p)) v))
        (/ 1 p)))

(define (vector-norm v)
  (p-vector-norm v))

(define (normalize-vector v)
  (vector-/-number v (vector-norm v)))

(define (zero-vector n)
  (make-vector n 0))
