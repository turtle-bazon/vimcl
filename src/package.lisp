(defpackage #:vimcl
  (:use #:cl #:iterate)
  (:local-nicknames (#:baz #:ru.bazon.cl-bazon))
    (:export #:make-empty-buffer
           #:buffer-p
           #:buffer-lines
           #:buffer-row
           #:buffer-col
           #:buffer-line-count
           #:buffer-current-line
           #:buffer-insert-char
           #:buffer-delete-char
           #:buffer-delete-backward
           #:buffer-split-line
           #:buffer-move
           #:vimcl-toplevel
           #:main)
)
