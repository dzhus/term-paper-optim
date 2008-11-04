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

(define start
  (new button%
       [label "Пшла!"] [parent frame]
       [stretchable-width #t]))

(send frame show #t)