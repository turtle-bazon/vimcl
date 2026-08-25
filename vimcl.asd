(asdf:defsystem "vimcl"
  :description "vi-inspired editor in Common Lisp"
  :version "0.0.1.0"
  :license "GPL-3.0"
  :author "turtle-bazon"
  :depends-on ("uiop" "clingon" "cl-bazon" "iterate" "cl-charms")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "buffer")
     (:file "editor")
     (:file "editor-keys")
     (:file "display")
     (:file "main"))))
  :build-operation "program-op"
  :build-pathname "build/vimcl"
  :entry-point "vimcl:main")
