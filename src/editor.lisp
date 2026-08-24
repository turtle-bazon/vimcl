(in-package #:vimcl)

(defun buffer-render (buf)
  "Render BUF for display: numbered lines, with a caret marking the
cursor column under the cursor row."
  (with-output-to-string (out)
    (loop for l in (buffer-lines buf)
          for i from 0
          do (format out "~a: ~a~%" i l)
             (when (= i (buffer-row buf))
               (format out "   ~a^~%"
                       (make-string (buffer-col buf)
                                    :initial-element #\Space))))))

(defun editor-step (buf line)
  "Apply one ed-style editor command LINE to BUF.
   Returns (values new-buf quit-p output) — OUTPUT is a string to
   display, or NIL. Pure: no I/O here, so the loop stays testable."
  (let ((cmd (string-trim '(#\Space #\Tab) line)))
    (flet ((one-char-command (prefix default)
             ;; "x" alone or "i CH" style: the operand char
             (if (> (length cmd) (1+ (length prefix)))
                 (char cmd (1+ (length prefix)))
                 default)))
      (cond
        ((string= cmd "q") (values buf t nil))
        ((string= cmd "") (values buf nil nil))
        ((string= cmd "h") (values (buffer-move buf 0 -1) nil))
        ((string= cmd "l") (values (buffer-move buf 0 1) nil))
        ((string= cmd "j") (values (buffer-move buf 1 0) nil))
        ((string= cmd "k") (values (buffer-move buf -1 0) nil))
        ((string= cmd "0") (values (buffer-move buf 0 (- (buffer-col buf))) nil))
        ((string= cmd "$") (values (buffer-move buf 0 999) nil))
        ((string= cmd "x") (values (buffer-delete-char buf) nil))
        ((string= cmd "X") (values (buffer-delete-backward buf) nil))
        ((string= cmd "o") (values (buffer-split-line buf) nil))
        ((string= cmd "%p") (values buf nil (buffer-render buf)))
        ((and (> (length cmd) 2) (string= cmd "i " :end1 2))
         (values (buffer-insert-char buf (char cmd 2)) nil))
        (t (values buf nil
                   (format nil "Unknown command: ~a (help lists commands)" cmd)))))))

(defun run-editor-session (buf &optional (stream *standard-input*) display)
  "Read editor commands from STREAM until quit/EOF.
   When DISPLAY is non-NIL (interactive use), render the buffer before
   each command so the user sees the effect of what they type."
  (loop
    (when display
      (write-line (buffer-render buf))
      (format t "vimcl> ")
      (force-output))
    (let ((line (read-line stream nil nil)))
      (unless line (return))
      (multiple-value-bind (new-buf quit-p output)
          (editor-step buf line)
        (setf buf new-buf)
        (when (and output (not display)) (write-line output))
        (when quit-p (return))))))

