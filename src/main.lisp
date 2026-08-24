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
   :handler #'vimcl-handler
   :sub-commands (list (make-edit-command))))

(defun main ()
  (let ((app (make-vimcl-command)))
    (clingon:run app)))
(defun edit-handler (cmd)
  (declare (ignore cmd))
  (format t "vimcl editor — ed-style commands: h j k l x X o i C %%p q~%")
  (run-editor-session (make-empty-buffer)))

(defun make-edit-command ()
  (clingon:make-command
   :name "edit"
   :description "Start an editing session on an empty buffer"
   :handler #'edit-handler))
