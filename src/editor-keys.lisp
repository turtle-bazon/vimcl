(in-package #:vimcl)


(defun editor-key (buf mode key)
  "Apply the normalized KEY to BUF in MODE (:normal or :insert).
   Returns (values new-buf new-mode message).
   MESSAGE is a status-line string or NIL. Pure: no I/O."
  (let ((norm (normalize-key key)))
    (if (and (consp norm) (eq (first norm) :char))
        (let ((ch (second norm)))
          (case mode
            (:insert (values (buffer-insert-char buf ch) mode nil))
            (t (values buf mode
                       (format nil "Press i to insert, : for commands (~a)" ch)))))
        (case mode
          (:normal
           (case norm
             (:left (values (buffer-move buf 0 -1) mode nil))
             (:right (values (buffer-move buf 0 1) mode nil))
             (:up (values (buffer-move buf -1 0) mode nil))
             (:down (values (buffer-move buf 1 0) mode nil))
             (:escape (values buf mode nil))
             (:enter (values (buffer-split-line buf) mode nil))
             (:backspace (values (buffer-delete-backward buf) mode nil))
             (t (values (buffer-delete-char buf) mode nil))))  ; x and unknowns
          (:insert
           (case norm
             (:escape (values buf :normal nil))
             (:enter (values (buffer-split-line buf) mode nil))
             (:backspace (values (buffer-delete-backward buf) mode nil))
             (t (values buf mode nil))))  ; arrows ignored in insert v1
          (t (values buf mode nil))))))
