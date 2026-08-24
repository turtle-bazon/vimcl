(in-package #:vimcl)

(defparameter +version+ "0.0.1.0")

(defun vimcl-version ()
  "vimcl 0.0.1.0")

(defun vimcl-handler (cmd)
  (declare (ignore cmd))
  (format t "vimcl ~a — vi-inspired editor in Common Lisp (skeleton)~%"
          +version+))

(defun make-vimcl-command ()
  (clingon:make-command
   :name "vimcl"
   :version +version+
   :description "vi-inspired editor in Common Lisp"
   :authors '("turtle-bazon")
   :license "GPL-3.0"
   :handler #'vimcl-handler))

(defun main ()
  (let ((app (make-vimcl-command)))
    (clingon:run app)))
