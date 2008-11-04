#lang scheme/gui

(require "relch.ss"
         "test-functions.ss")

(define frame
  (new frame%
       [label "Демонстрация метода оптимизации с чебышёвской функцией релаксации"]
       [width 500] [height 500]
       [alignment '(left top)]))

(define log
  (new text-field%
       [label "Протокол работы:"] [parent frame] [style '(multiple vertical-label)]))

(define choice
  (new choice%
       [label "Тестовая функция:"]
       [parent frame] [choices (map test-function-name test-functions)]))

(define (log-poster msg)
  (send (send log get-editor) insert msg))

(define (start-optimization choice)
  (let ((f (list-ref test-functions choice)))
    (let ((x (optimize (test-function-def f)
                       (test-function-x-start f))))
      (log-poster (format "~a" x)))))

(define start
  (new button%
       [label "Пшла!"] [parent frame]
       [callback (lambda (i e)
                   (start-optimization (send choice get-selection)))]
       [stretchable-width #t]))

(send frame show #t)