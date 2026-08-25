(in-package #:vimcl)

(defun normalize-key (key)
  "Map a charms key to a keyword or (list :char CH)."
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
  (let ((norm (normalize-key key)))
    (cond
      ;; vim: Esc in insert moves the cursor one left on exit
      ((eq norm :escape)
       (values (if (> (buffer-col buf) 0)
                   (buffer-move buf 0 -1)
                   buf)
               :normal nil))
      ((eq norm :enter) (values (buffer-split-line buf) :insert nil))
      ((eq norm :backspace) (values (buffer-delete-backward buf) :insert nil))
      ((and (consp norm) (eq (first norm) :char))
       (values (buffer-insert-char buf (second norm)) :insert nil))
      ;; arrows navigate without leaving insert mode
      ((eq norm :left) (values (buffer-move buf 0 -1) :insert nil))
      ((eq norm :right) (values (buffer-move buf 0 1) :insert nil))
      ((eq norm :up) (values (buffer-move buf -1 0) :insert nil))
      ((eq norm :down) (values (buffer-move buf 1 0) :insert nil))
      (t (values buf :insert nil)))))


(defun editor-key-normal (buf key)
  (let* ((norm (normalize-key key))
         (ch (if (and (consp norm) (eq (first norm) :char))
                 (second norm) nil)))
    (cond
      ((and ch (eql ch #\h)) (values (buffer-move buf 0 -1) :normal nil))
      ((and ch (eql ch #\j)) (values (buffer-move buf 1 0) :normal nil))
      ((and ch (eql ch #\k)) (values (buffer-move buf -1 0) :normal nil))
      ((and ch (eql ch #\l)) (values (buffer-move buf 0 1) :normal nil))
      ((and ch (eql ch #\0)) (values (buffer-set-cursor buf (buffer-row buf) 0) :normal nil))
      ;; vim: $ lands ON the last character, not past it
      ;; land on the last char, curswant to infinity so chained
      ;; j/k stick to end-of-line like real vim
      ((and ch (eql ch #\$))
       (values (buffer-set-cursor buf (buffer-row buf)
                                   (max 0 (1- (length (buffer-current-line buf))))
                                   9999)
               :normal nil))
      ((and ch (eql ch #\x)) (values (buffer-delete-char buf) :normal nil))
      ((and ch (eql ch #\i)) (values buf :insert nil))
      ((and ch (eql ch #\a)) (values (buffer-move buf 0 1) :insert nil))
      ((and ch (eql ch #\A)) (values (buffer-move buf 0 9999) :insert nil))
      ((and ch (eql ch #\I)) (values (buffer-set-cursor buf (buffer-row buf) 0) :insert nil))
      ((and ch (eql ch #\o)) (values (buffer-open-line-below buf) :insert nil))
      ((and ch (eql ch #\O)) (values (buffer-open-line-above buf) :insert nil))
      ;; vim: G goes to the LAST line, column reset to zero
      ((and ch (eql ch #\G))
       (values (buffer-set-cursor buf (1- (length (buffer-lines buf))) 0)
               :normal nil))
      ((eq norm :left) (values (buffer-move buf 0 -1) :normal nil))
      ((eq norm :right) (values (buffer-move buf 0 1) :normal nil))
      ((eq norm :up) (values (buffer-move buf -1 0) :normal nil))
      ((eq norm :down) (values (buffer-move buf 1 0) :normal nil))
      ((eq norm :escape) (values buf :normal nil))
      ((eq norm :enter) (values (buffer-split-line buf) :normal nil))
      ((eq norm :backspace) (values (buffer-delete-backward buf) :normal nil))
      (t (values buf :normal nil)))))



(defun editor-key (buf mode key)
  (case mode
    (:insert (editor-key-insert buf key))
    (t (editor-key-normal buf key))))
