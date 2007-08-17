
(in-package :cl-user)

(defpackage :dll
  (:use :cl)
  (:export :make-cell
           :data
           :right
           :left
           :insert-before
           :insert-after
           :to-right
           :to-left
           :length
           :->list
           :with-data))

