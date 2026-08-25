(in-package #:vimcl)

(defun draw-editor (win buf mode message)
  "Paint the whole editor screen: buffer lines, status line, cursor.
   Uses only the high-level charms API."
  (charms:clear-window win)
  ;; window-dimensions returns (values WIDTH HEIGHT) — bind accordingly
  (multiple-value-bind (cols rows) (charms:window-dimensions win)
    (declare (ignore cols))
    (loop for line in (buffer-lines buf)
          for i from 0
          while (< i (1- rows))
          ;; ncurses mvwaddstr signals ERR for empty strings
          unless (zerop (length line))
            do (charms:write-string-at-point win line 0 i))
    (let ((status (format nil "~a [~a ~a:~a]"
                          (or message "")
                          (ecase mode (:normal "NORMAL") (:insert "INSERT"))
                          (buffer-row buf) (buffer-col buf))))
      (charms:write-string-at-point win status 0 (1- rows)))
    ;; park the ncurses cursor at the buffer cursor position by
    ;; re-writing the character under it
    (let* ((cur (buffer-current-line buf))
           (col (min (buffer-col buf) (length cur)))
           (row (min (buffer-row buf) (- rows 2)))
           (under (if (< col (length cur))
                      (string (char cur col))
                      " ")))
      (charms:write-string-at-point win under col row))
    (charms:refresh-window win)))

(defun handle-ex-key (buf ex-acc key file)
  "One keystroke in ex mode. Returns
   (values buf new-ex-acc new-mode quit-p message)."
  (let ((norm (normalize-key key)))
    (cond
      ((eq norm :escape)
       (values buf nil :normal nil nil))
      ((eq norm :enter)
       (let ((cmd (string-trim '(#\Space) ex-acc)))
         (cond
           ((string= cmd "w") (write-buffer-to-file buf file)
                              (values buf nil :ex nil "written"))
           ((string= cmd "q") (values buf nil :ex t nil))
           ((string= cmd "wq") (write-buffer-to-file buf file)
                               (values buf nil :ex t "written"))
           ((string= cmd "q!") (values buf nil :ex t nil))
           (t (values buf nil :ex nil
                      (format nil "Unknown ex command: ~a" cmd))))))
      ((and (consp norm) (eq (first norm) :char))
       (values buf (concatenate 'string ex-acc
                                (string (second norm)))
               :ex nil nil))
      (t (values buf ex-acc :ex nil nil)))))

(defun run-fullscreen-editor (buf &optional file)
  "Full-screen modal editor over BUF. FILE is the persistence target
   for :w/:wq ex commands."
  (charms:with-curses ()
    ;; low-level raw/noecho: the high-level enable-raw-input crashes on
    ;; fresh init here (its internal nocbreak returns ERR when ncurses
    ;; starts with no input mode set — cl-charms quirk in this env)
    (cl-charms/low-level:raw)
    (cl-charms/low-level:noecho)
    (charms:enable-extra-keys charms:*standard-window*)
    (let* ((win charms:*standard-window*)
           (mode :normal)
           (ex-acc nil)
           (message nil))
      (loop
        (draw-editor win buf mode message)
        (setf message nil)
        (let ((key (charms:get-char win :ignore-error t)))
          (if (eq mode :ex)
              (multiple-value-bind (nb nacc nm quit msg)
                  (handle-ex-key buf ex-acc key file)
                (setf buf nb ex-acc nacc mode nm message msg)
                (setf ex-acc (if (eq mode :ex) nacc nil))
                (when quit (return)))
              (multiple-value-bind (nb nm msg)
                  (editor-key buf mode key)
                (setf buf nb mode nm message msg)
                (with-open-file (o "/tmp/vimcl-mode.txt" :direction :output
                                    :if-exists :supersede)
                  (format o "post-setf mode=~s nm=~s~%" mode nm))
                (when (eq mode :ex) (setf ex-acc "")))))))))
