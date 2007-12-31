
(in-package :cl-user)

;; This version of map keeps track of the tail of the list, so it doesn't have
;; to nreverse at the end.
(defun my-map (fn input)
  (if (consp input)
    (let* ((hd (cons (funcall fn (car input)) nil))
           (cursor hd))
      (do ((input (cdr input) (cdr input)))
        ((null input) hd)
        (setf (cdr cursor) (cons (funcall fn (car input)) nil))
        (setf cursor (cdr cursor))))
    nil))

