(in-package #:vimcl)

(defstruct (buffer (:constructor %make-buffer (lines row col)))
  "A text buffer: LINES is a list of strings; the cursor sits at
0-based ROW/COL. All operations are pure — they return new buffers."
  lines row col)

(defun make-empty-buffer ()
  "A buffer holding a single empty line, cursor at origin."
  (%make-buffer (list "") 0 0))

(defun buffer-line-count (buf)
  "Number of lines in BUF."
  (length (buffer-lines buf)))

(defun buffer-current-line (buf)
  "The line under the cursor."
  (nth (buffer-row buf) (buffer-lines buf)))

(defun clamp-row (lines row)
  (max 0 (min row (1- (length lines)))))

(defun clamp-col (line col)
  (max 0 (min col (length line))))

(defun replace-nth (list idx new-elt)
  "Return LIST with element at IDX replaced by NEW-ELT."
  (loop for e in list
        for i from 0
        collect (if (= i idx) new-elt e)))

(defun buffer-insert-char (buf ch)
  "Insert CH at the cursor; the cursor advances past it."
  (let* ((row (buffer-row buf))
         (line (buffer-current-line buf))
         (col (buffer-col buf))
         (new-line (concatenate 'string
                                (subseq line 0 col)
                                (string ch)
                                (subseq line col))))
    (%make-buffer (replace-nth (buffer-lines buf) row new-line)
                  row (1+ col))))

(defun buffer-delete-char (buf)
  "Delete the character under the cursor. No-op at end of line."
  (let* ((row (buffer-row buf))
         (line (buffer-current-line buf))
         (col (buffer-col buf)))
    (if (< col (length line))
        (%make-buffer
         (replace-nth (buffer-lines buf) row
                      (concatenate 'string
                                   (subseq line 0 col)
                                   (subseq line (1+ col))))
         row col)
        buf)))

(defun buffer-delete-backward (buf)
  "Delete the character before the cursor. At column 0, join the
current line onto the previous one (cursor at the join point).
No-op in the top-left corner."
  (let* ((row (buffer-row buf))
         (col (buffer-col buf))
         (lines (buffer-lines buf)))
    (cond
      ((and (= row 0) (= col 0)) buf)
      ((= col 0)
       (let* ((prev (nth (1- row) lines))
              (join-col (length prev))
              (joined (concatenate 'string prev (nth row lines))))
         (%make-buffer (append (subseq lines 0 (1- row))
                               (list joined)
                               (subseq lines (1+ row)))
                       (1- row) join-col)))
      (t
       (%make-buffer
        (replace-nth lines row
                     (concatenate 'string
                                  (subseq line 0 (1- col))
                                  (subseq line col)))
        row (1- col))))))

(defun buffer-split-line (buf)
  "Break the line at the cursor (ENTER); cursor moves to the new
line's start."
  (let* ((row (buffer-row buf))
         (line (buffer-current-line buf))
         (col (buffer-col buf))
         (head (subseq line 0 col))
         (tail (subseq line col)))
    (%make-buffer (append (subseq (buffer-lines buf) 0 row)
                          (list head tail)
                          (subseq (buffer-lines buf) (1+ row)))
                  (1+ row) 0)))

(defun buffer-move (buf dr dc)
  "Move the cursor by DR/DC, clamped to the buffer."
  (let* ((lines (buffer-lines buf))
         (new-row (clamp-row lines (+ (buffer-row buf) dr)))
         (new-line (nth new-row lines))
         (new-col (clamp-col new-line (+ (buffer-col buf) dc))))
    (%make-buffer lines new-row new-col)))
