(in-package #:vimcl)

(defun draw-editor (win st)
  "Paint the screen like vanilla vim: buffer lines, ~ fillers, and a
right-anchored bottom row [banner][pad][showcmd x10][ruler][4sp][msg]
with one reserved corner column. Drawn with raw mvwaddstr because the
high-level writer aborts when the trailing cursor touches the wrap
column."
  (charms:clear-window win)
  (multiple-value-bind (cols rows) (charms:window-dimensions win)
    (let* ((buf (editor-state-buf st))
           (lines (buffer-lines buf))
           (nlines (length lines))
           (text-rows (- rows 1))
           (mode (editor-state-mode st))
           (cur-row (if (zerop nlines)
                        0
                        (min (buffer-row buf) (1- nlines))))
           (cur-line (or (nth cur-row lines) ""))
           (len (length cur-line))
           (max-col (if (eq mode :insert) len (max 0 (1- len))))
           (col (min (buffer-col buf) max-col)))
      ;; text and ~ filler lines
      (loop for i from 0 below text-rows
            do (if (< i nlines)
                   (let ((line (nth i lines)))
                     (unless (zerop (length line))
                       (charms:write-string-at-point win line 0 i)))
                   (unless (or (= i (1- rows))
                        (and (zerop nlines) (zerop i)))
                     (charms:write-string-at-point win "~" 0 i))))
      ;; compose the status line
      (let* ((view-msg (cond
                        ((<= nlines text-rows) "All")
                        ((zerop cur-row) "Top")
                        ((= cur-row (1- nlines)) "Bot")
                        (t (format nil "~a%"
                                   (round (* 100 (+ cur-row 1)) nlines)))))
             (ruler-str (if (zerop nlines)
                            "0, 0-1"
                            (format nil "~a, ~a" (1+ cur-row)
                                (if (zerop len) "0-1" (1+ col)))))
             (showcmd-str (or (editor-state-showcmd st) ""))
             (raw-msg (cond
                        ((editor-state-message st)
                         (princ-to-string (editor-state-message st)))
                        ((eq mode :insert) "-- INSERT --")))
             ;; vim middle-truncates overlong messages so the ruler
             ;; stays anchored (msg budget ~= cols - 29)
             (msg-budget (max 0 (- cols 29)))
             (msg-str (when raw-msg
                        (if (> (length raw-msg) msg-budget)
                            (let* ((keep (- msg-budget 3))
                                   (h (ceiling keep 2))
                                   (tl (floor keep 2)))
                              (concatenate 'string
                                           (subseq raw-msg 0 h)
                                           "..."
                                           (subseq raw-msg
                                                   (- (length raw-msg) tl))))
                            raw-msg)))
             (banner (when (and (editor-state-show-info st)
                                (buffer-filename buf))
                       (format nil "\"~a\" ~aL,~aB"
                               (buffer-filename buf) nlines
                               (or (editor-state-shown-size st)
                                   (buffer-size buf)))))
             (ruler-start (max 0 (- cols 18)))
             (pad-len (max 0 (- ruler-start (length showcmd-str))))
             (left (cond (msg-str msg-str)
                         (banner banner)
                         (t "")))
             (left-clipped (if (> (length left) pad-len)
                               (subseq left 0 pad-len)
                               left))
             (status (concatenate 'string
                                  left-clipped
                                  (make-string (- pad-len
                                                  (length left-clipped))
                                               :initial-element #\Space)
                                  showcmd-str
                                  ruler-str
                                  (make-string
                                   (max 0 (- (- cols 10)
                                             (+ ruler-start
                                                (length ruler-str))))
                                   :initial-element #\Space)
                                  view-msg))
             (safe (subseq status 0 (min (length status)
                                         (max 0 (1- cols))))))
        (unless (zerop (length safe))
          (cl-charms/low-level:mvwaddstr
           (charms::window-pointer win) (1- rows) 0 safe)))
      ;; park the ncurses cursor at the buffer position
      (cl-charms/low-level:wmove
       (charms::window-pointer win) cur-row col)
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
  "Full-screen modal editor driven by the EDITOR-STATE command
grammar (usr_04). FILE is the persistence target for ex commands."
  ;; vim-like fast escape handling: ncurses waits ESCDELAY ms to
  ;; disambiguate a lone Esc from escape sequences (default 1000!)
  (cffi:foreign-funcall "setenv" :string "ESCDELAY"
                        :string "30" :int 1 :int)
  (charms:with-curses ()
    (cl-charms/low-level:raw)
    (cl-charms/low-level:noecho)
    (charms:enable-extra-keys charms:*standard-window*)
    (let* ((win charms:*standard-window*)
           (prev-nlines (length (buffer-lines buf)))
           (st (make-editor-state
                :buf buf :mode :normal :ex-acc nil
                :shown-size (buffer-size buf))))
      (declare (ignorable file))
      (loop
        (draw-editor win st)
        ;; vim keeps the message until replaced or mode changes; the
        ;; handlers below overwrite it explicitly
        (let ((key (charms:get-char win :ignore-error t)))
          (if (eq (editor-state-mode st) :ex)
              (multiple-value-bind (nb nacc nm quit msg)
                  (handle-ex-key (editor-state-buf st)
                                 (editor-state-ex-acc st) key
                                 (buffer-filename (editor-state-buf st)))
                (setf (editor-state-buf st) nb
                      (editor-state-ex-acc st)
                      (if (eq nm :ex) nacc nil)
                      (editor-state-mode st) nm
                      (editor-state-message st) msg)
                (when msg
                  (setf (editor-state-shown-size st)
                        (buffer-size (editor-state-buf st))))
                (when quit (return)))
              ;; insert or normal
              (progn
                (if (eq (editor-state-mode st) :insert)
                    (insert-step st key)
                    (normal-step st key))
                ;; banner lifetime: dies on first mode change or message
                (let ((nl (length (buffer-lines (editor-state-buf st)))))
                  (when (and (editor-state-show-info st)
                             (or (editor-state-message st)
                                 (not (eq (editor-state-mode st) :normal))
                                 (/= nl prev-nlines)))
                    (setf (editor-state-show-info st) nil))
                  (setf prev-nlines nl))
                (when (eq (editor-state-mode st) :ex)
                  (setf (editor-state-ex-acc st) "")))))))))

