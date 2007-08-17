
(in-package :dll)

(defun make-cell (data &key left right)
  "Create a double-linked list cell"
  (cons data (cons left right)))

(defun data (cell)
  "Retrieve the data from a double-linked list cell"
  (car cell))

(defun (setf data) (value cell)
  "Set the data for a double-linked list cell"
  (setf (car cell) value))

(defun left (cell)
  "Retrieve the preceding cell or nil"
  (cadr cell))

(defun (setf left) (value cell)
  "Set the preceding cell"
  (setf (cadr cell) value))

(defun right (cell)
  "Retrieve the next cell or nil"
  (cddr cell))

(defun (setf right) (value cell)
  "Set the next cell"
  (setf (cddr cell) value))

(defun insert-after (cell data)
  "Insert a new cell containing data after the given cell"
  (let* ((right-cell (right cell))
         (new-cell (make-cell data cell right-cell)))
    (when right-cell
      (setf (left right-cell) new-cell))
    (setf (right cell) new-cell)
    new-cell))

(defun insert-before (cell data)
  "Insert a new cell containing data before the given cell"
  (let* ((left-cell (left cell))
         (new-cell (make-cell data left-cell cell)))
    (when left-cell
      (setf (right left-cell) new-cell))
    (setf (left cell) new-cell)
    new-cell))

(defun to-right (cell)
  "Return the right-most cell from cell"
  (if (right cell)
    (to-right (right cell))
    cell))

(defun to-left (cell)
  "Return the left-most cell from cell"
  (if (left cell)
    (to-left (left cell))
    cell))

(defun length (cell)
  "Return the length of the double-linked list to the right"
  (loop for c = cell then (right cell)
        while c
        count 1))

(defun ->list (cell)
  "Returns a list containing the values from cell to the right"
  (loop for c = cell then (right c)
        while c
        collect (data c)))

(defmacro with-data ((var start-cell) &body body)
  "Execute body with var bound to the data from successive cells, starting
  with start-cell and traveling right"
  (let ((result (gensym))
        (cell (gensym)))
    `(let (,result)
       (do ((,cell ,start-cell (right ,cell)))
         ((null ,cell) ,result)
         (let ((,var (data ,cell)))
           (setf ,result (progn ,@body)))))))

