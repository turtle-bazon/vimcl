(asdf:defsystem "vimcl-tests"
  :description "Tests for vimcl"
  :depends-on (#:vimcl #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "buffer-test")))))
