(in-package #:vimcl)

(defstruct (buffer (:constructor %%make-buffer (lines row col filename want-col)))
  "A text buffer: LINES is a list of strings; the cursor sits at
0-based ROW/COL. WANT-COL is the desired column (vim curswant):
vertical moves land on WANT-COL clamped to the new line, so after $
every j/k sticks to end-of-line. FILENAME is the persistence target.
All operations are pure."
  lines row col
  (filename nil :read-only t)
  (want-col 0))

(defun %make-buffer (lines row col &optional (filename nil) (want-col :unset))
  "Constructor shim: curswant defaults to the actual column, which is
what vim does for every editing operation except explicit motions."
  (%%make-buffer lines row col filename
                 (if (eq want-col :unset) col want-col)))

(defun make-empty-buffer (&optional filename)
  (%make-buffer (list "") 0 0 filename))

(defun buffer-from-file (file)
  (if (and file (probe-file file))
      (with-open-file (s file :direction :input)
        (%make-buffer (loop for line = (read-line s nil nil)
                            while line
                            collect line)
                      0 0 file))
      (make-empty-buffer file)))

(defun write-buffer-to-file (buf file)
  (with-open-file (s file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (dolist (l (buffer-lines buf))
      (write-line l s))))

(defun buffer-line-count (buf)
  (length (buffer-lines buf)))

(defun buffer-current-line (buf)
  (or (nth (buffer-row buf) (buffer-lines buf)) ""))

(defun clamp-row (lines row)
  (max 0 (min row (1- (length lines)))))

(defun clamp-col (line col)
  (max 0 (min col (length line))))

(defun replace-nth (list idx new-elt)
  (loop for e in list for i from 0
        collect (if (= i idx) new-elt e)))

(defun buffer-set-line (buf idx new-line)
  (%make-buffer (replace-nth (buffer-lines buf) idx new-line)
                (buffer-row buf) (buffer-col buf)
                (buffer-filename buf)))

(defun buffer-set-cursor (buf row col &optional (want col))
  (%make-buffer (buffer-lines buf) row col (buffer-filename buf) want))

(defun buffer-insert-char (buf ch)
  "Insert CH at the cursor; the cursor advances past it."
  (let* ((row (max 0 (min (buffer-row buf)
                          (1- (max 1 (buffer-line-count buf))))))
         (line (buffer-current-line buf))
         (col (buffer-col buf))
         (new-line (concatenate 'string
                                (subseq line 0 col)
                                (string ch)
                                (subseq line col))))
    (%make-buffer (replace-nth (buffer-lines buf) row new-line)
                  row (1+ col)
                  (buffer-filename buf))))


(defun buffer-delete-char (buf)
  (let* ((row (buffer-row buf))
         (line (buffer-current-line buf))
         (col (buffer-col buf)))
    (if (< col (length line))
        (buffer-set-line buf row
                         (concatenate 'string
                                      (subseq line 0 col)
                                      (subseq line (1+ col))))
        buf)))

(defun buffer-delete-backward (buf)
  (let* ((row (buffer-row buf))
         (col (buffer-col buf))
         (lines (buffer-lines buf)))
    (cond
      ((and (= row 0) (= col 0)) buf)
      ((= col 0)
       (let* ((prev (nth (1- row) lines))
              (cur (nth row lines))
              (join-col (length prev))
              (joined (concatenate 'string prev cur)))
         (%make-buffer
          (append (subseq lines 0 (1- row))
                  (list joined)
                  (subseq lines (1+ row)))
          (1- row) join-col (buffer-filename buf))))
      (t
       (buffer-set-line buf row
                        (concatenate 'string
                                     (subseq line 0 (1- col))
                                     (subseq line col)))))))

(defun buffer-split-line (buf)
  (let* ((row (buffer-row buf))
         (line (buffer-current-line buf))
         (col (buffer-col buf))
         (head (subseq line 0 col))
         (tail (subseq line col)))
    (%make-buffer (append (subseq (buffer-lines buf) 0 row)
                          (list head tail)
                          (subseq (buffer-lines buf) (1+ row)))
                  (1+ row) 0 (buffer-filename buf))))

(defun buffer-move (buf dr dc)
  "Vim-style motion: horizontal moves update both column and
curswant; vertical moves land on curswant clamped to the new line,
preserving curswant for chained j/k (so $-then-k stays at eol)."
  (let* ((lines (buffer-lines buf))
         (new-row (clamp-row lines (+ (buffer-row buf) dr))))
    (if (zerop dc)
        ;; vertical: target column comes from want-col
        (let ((new-line (nth new-row lines)))
          (buffer-set-cursor buf new-row
                             (clamp-col new-line (buffer-want-col buf))
                             (buffer-want-col buf)))
        ;; horizontal: column and curswant move together
        (let ((new-col (clamp-col (nth new-row lines)
                                   (+ (buffer-col buf) dc))))
          (buffer-set-cursor buf new-row new-col new-col)))))

(defun buffer-open-line-below (buf)
  (let* ((row (buffer-row buf))
         (lines (if (null (buffer-lines buf))
                    (list "")
                    (buffer-lines buf)))
         (row (max 0 (min row (1- (length lines))))))
    (%make-buffer (append (subseq lines 0 (1+ row))
                          (list "")
                          (subseq lines (1+ row)))
                  (1+ row) 0 (buffer-filename buf))))

(defun buffer-open-line-above (buf)
  (let* ((row (buffer-row buf))
         (lines (if (null (buffer-lines buf))
                    (list "")
                    (buffer-lines buf)))
         (row (max 0 (min row (1- (length lines))))))
    (%make-buffer (append (subseq lines 0 row)
                          (list "")
                          (subseq lines row))
                  row 0 (buffer-filename buf))))

(defun buffer-delete-line (buf)
  (let* ((row (buffer-row buf))
         (lines (buffer-lines buf)))
    (if (= (length lines) 1)
        (make-empty-buffer (buffer-filename buf))
        (%make-buffer (append (subseq lines 0 row)
                              (subseq lines (1+ row)))
                      (clamp-row (append (subseq lines 0 row)
                                          (subseq lines (1+ row)))
                                  row)
                      0 (buffer-filename buf)))))
(defun buffer-size (buf)
  "Size of the buffer as it would be written to disk:
   one newline terminator per stored line (vim byte-count parity)."
  (+ (length (buffer-lines buf))
     (reduce (lambda (acc line) (+ acc (length line)))
             (buffer-lines buf) :initial-value 0)))
