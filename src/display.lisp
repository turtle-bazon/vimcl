(in-package #:vimcl)

(defun draw-editor (win buf mode message)
  "Paint the whole editor screen: buffer lines, status line, cursor."
  (cl-charms/low-level:werase win)
  (let ((maxy (1- (cl-charms/low-level:getmaxy win))))
    (loop for line in (buffer-lines buf)
          for i from 0
          while (< i maxy)
          do (cl-charms/low-level:wmove win i 0)
             (cl-charms/low-level:waddstr win line))
    (cl-charms/low-level:wmove win maxy 0)
    (cl-charms/low-level:waddstr
     win (format nil "~a [~a ~a:~a]"
                 (or message "")
                 (ecase mode (:normal "NORMAL") (:insert "INSERT"))
                 (buffer-row buf) (buffer-col buf)))
    (cl-charms/low-level:wmove win
                               (min (buffer-row buf) (1- maxy))
                               (buffer-col buf))
    (cl-charms/low-level:wrefresh win)))

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
    (charms:enable-raw-input :interpret-control-characters)
    (cl-charms/low-level:noecho)
    (cl-charms/low-level:clear)
    (let ((win charms:*standard-window*)
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
                (when quit (return)))
              (multiple-value-bind (nb nm msg)
                  (editor-key buf mode key)
                (setf buf nb mode nm message msg)
                (when (eq mode :ex) (setf ex-acc "")))))))))
