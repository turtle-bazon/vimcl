(in-package #:vimcl)

(defun draw-editor (win buf mode message &optional (shown-size nil) (show-info t))
  "Paint the whole editor screen vim-style: buffer lines, ~ fillers
   below EOF, vim-format status line, cursor parked at buffer position."
  (charms:clear-window win)
  (multiple-value-bind (cols rows) (charms:window-dimensions win)
    (let* ((lines (buffer-lines buf))
           (nlines (length lines))
           (text-rows (- rows 1)))
      ;; buffer text, then ~ filler lines like vim
      (loop for i from 0 below text-rows
            do (if (< i nlines)
                   (let ((line (nth i lines)))
                     (unless (zerop (length line))
                       (charms:write-string-at-point win line 0 i)))
                   (charms:write-string-at-point win "~" 0 i)))
      ;; status line: vim shows "-- INSERT --" + ruler in insert;
      ;; "file" NL, BB + ruler in normal; messages take precedence
      (let* ((pos (let* ((crow (min (buffer-row buf) (1- nlines)))
                     (cline (nth crow lines))
                     (clen (length cline))
                     (cmax (if (eq mode :insert) clen (max 0 (1- clen)))))
                (format nil "~a, ~a" (1+ crow)
                        (1+ (min (buffer-col buf) cmax)))))
             (cur-row (min (buffer-row buf) (1- nlines)))
(view-msg (cond
           ((<= nlines text-rows) "All")
           ((zerop cur-row) "Top")
           ((= cur-row (1- nlines)) "Bot")
           (t (format nil "~a%" (round (* 100 (+ cur-row 1)) nlines)))))
(info (cond
        (message (princ-to-string message))
        ((eq mode :insert) "-- INSERT --")
        ((and show-info (buffer-filename buf)
                (format nil "\"~a\" ~aL, ~aB"
                        (buffer-filename buf)
                        nlines
                        (or shown-size (buffer-size buf)))))))
(status (format nil "~@[~a~]~60T~a~67T~a" info pos view-msg)))
        (charms:write-string-at-point win status 0 (1- rows)))
      ;; park the real ncurses cursor at the buffer position,
      ;; clamped like vim: normal mode sits ON the last char,
      ;; insert mode may sit one past it
      (let* ((cur-row (min (buffer-row buf) (1- nlines)))
             (cur-line (nth cur-row lines))
             (len (length cur-line))
             (max-col (if (eq mode :insert) len (max 0 (1- len))))
             (col (min (buffer-col buf) max-col)))
        (cl-charms/low-level:wmove
         (charms::window-pointer win) cur-row col))
      (charms:refresh-window win))))


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
           (message nil)
           ;; vim shows the byte count from load time, not live
           (shown-size (buffer-size buf))
           ;; vim prints the "file" NL, BB banner once, then only after writes
           (show-info t))
      (loop
        (draw-editor win buf mode message shown-size show-info)
        (setf message nil)
        (let ((key (charms:get-char win :ignore-error t)))
          (if (eq mode :ex)
              (multiple-value-bind (nb nacc nm quit msg)
                  (handle-ex-key buf ex-acc key file)
                (setf buf nb ex-acc nacc mode nm message msg)
                (setf ex-acc (if (eq mode :ex) nacc nil))
                (when quit (return))
                (when msg (setf shown-size (buffer-size buf) show-info t)))
              (multiple-value-bind (nb nm msg)
                  (editor-key buf mode key)
                (setf buf nb mode nm message msg)
                ;; banner dies on first mode change or message, like vim
                (when (or msg (not (eq nm :normal)))
                  (setf show-info nil))
                (when (eq mode :ex) (setf ex-acc "")))))))))
