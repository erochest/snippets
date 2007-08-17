
(in-package :cl-user)

(defpackage :dll-asd
  (:use :cl :asdf))

(in-package :dll-asd)

(defsystem :dll
           :serial t
           :components ((:file "packages")
                        (:file "dll")))

; vim: set filetype=lisp:
