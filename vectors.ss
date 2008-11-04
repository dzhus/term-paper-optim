#lang scheme

(require srfi/43)

(provide vector-sum)

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
