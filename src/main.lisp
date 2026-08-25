(in-package #:vimcl)

(defparameter +version+ "0.0.1.0")

(defun vimcl-version ()
  "vimcl 0.0.1.0")

(defun vimcl-handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (file (first args))
         (buf (if file
                  (buffer-from-file file)
                  (make-empty-buffer))))
    (run-fullscreen-editor buf file)))




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


