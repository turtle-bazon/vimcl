(in-package #:vimcl)


(defun normalize-key (key)
  "Map a charms key to a keyword or (list :char CH) for printables."
  (if (characterp key)
      (case key
        (#\Escape :escape)
        (#\Newline :enter)
        (#\Return :enter)
        (#\Del :backspace)
        (#\Rubout :backspace)
        (t (list :char key)))
      (case key
        (27 :escape)
        (13 :enter)
        (10 :enter)
        (127 :backspace)
        (263 :backspace)
        (260 :left)
        (261 :right)
        (259 :up)
        (258 :down)
        (t nil))))


(defun editor-key-insert (buf key)
  "INSERT mode: printable inserts, Enter splits, Backspace deletes,
   Esc returns to normal. Returns (values buf mode message)."
  (let ((norm (normalize-key key)))
    (cond
      ((eq norm :escape) (values buf :normal nil))
      ((eq norm :enter) (values (buffer-split-line buf) :insert nil))
      ((eq norm :backspace) (values (buffer-delete-backward buf) :insert nil))
      ((and (consp norm) (eq (first norm) :char))
       (values (buffer-insert-char buf (second norm)) :insert nil))
      (t (values buf :insert nil)))))


(defun editor-key-normal (buf key)
  "NORMAL mode: hjkl move, 0/$ line ends, x delete, i insert,
   o open line below. Returns (values buf mode message)."
  (let ((norm (normalize-key key)))
    (cond
      ((eq norm :left) (values (buffer-move buf 0 -1) :normal nil))
      ((eq norm :right) (values (buffer-move buf 0 1) :normal nil))
      ((eq norm :up) (values (buffer-move buf -1 0) :normal nil))
      ((eq norm :down) (values (buffer-move buf 1 0) :normal nil))
      ((eq norm :escape) (values buf :normal nil))
      ((eq norm :enter) (values (buffer-split-line buf) :normal nil))
      ((eq norm :backspace) (values (buffer-delete-backward buf) :normal nil))
      ((and (consp norm) (eq (first norm) :char))
       (let ((ch (second norm)))
         (case ch
           (#\h (values (buffer-move buf 0 -1) :normal nil))
           (#\j (values (buffer-move buf 1 0) :normal nil))
           (#\k (values (buffer-move buf -1 0) :normal nil))
           (#\l (values (buffer-move buf 0 1) :normal nil))
           (#\0 (values (buffer-move buf 0 (- (buffer-col buf))) :normal nil))
           (#\$ (values (buffer-move buf 0 9999) :normal nil))
           (#\x (values (buffer-delete-char buf) :normal nil))
           (#\i (values buf :insert nil))
           (#\o (values (buffer-split-line buf) :insert nil))
           (t (values buf :normal
                      (format nil "Normal command: ~c (h j k l 0 $ x i o)" ch))))))
      (t (values buf :normal nil)))))


(defun editor-key (buf mode key)
  "Dispatch KEY to the MODE-specific handler.
   Returns (values new-buf new-mode message). Pure: no I/O."
  (case mode
    (:insert (editor-key-insert buf key))
    (t (editor-key-normal buf key))))
