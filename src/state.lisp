(in-package #:vimcl)

;;; Editor state machine — mirrors vim's Normal-mode command grammar:
;;; {count?}{operator}{count?}{motion} | {count?}{simple-command}
;;; See usr_04 (operators/motions), motion.txt semantics implemented
;;; in word-motion helpers below.

(defstruct editor-state
  (buf nil)
  (mode :normal)
  (message nil)
  (ex-acc nil)
  ;; pending operator: :d :c :y or NIL
  (pending-op nil)
  ;; accumulated count digits (integer) for the command being built
  (pending-count nil)
  ;; operator-local count when given separately (3d2w -> 6 words)
  (op-count nil)
  ;; pending argument char for r/f/F/t/T commands
  (pending-arg-for nil)
  ;; unnamed register: (text . linewise-p) where text is a list of strings
  (register nil)
  ;; undo/redo stacks: entries are (lines row col want-col) snapshots
  (undo nil)
  (redo nil)
  ;; what to render in the showcmd field (string or nil)
  (showcmd nil)
  ;; vim: file banner shown until first mode change or message
  (show-info t)
  ;; byte count frozen at load time like vim's "NL, BB"
  (shown-size nil))

(defun snapshot-buffer (buf)
  "Immutable undo record of BUF."
  (list (copy-tree (buffer-lines buf))
        (buffer-row buf) (buffer-col buf) (buffer-want-col buf)))

(defun restore-snapshot (buf snap)
  (%make-buffer (copy-tree (first snap))
                (second snap) (third snap)
                (buffer-filename buf)
                (fourth snap)))

(defun push-undo (st)
  (push (snapshot-buffer (editor-state-buf st)) (editor-state-undo st))
  (setf (editor-state-redo st) nil))

(defun do-undo (st)
  (let ((stack (editor-state-undo st)))
    (if stack
        (progn
          (push (snapshot-buffer (editor-state-buf st))
                (editor-state-redo st))
          (let ((snap (pop stack)))
            (setf (editor-state-undo st) stack
                  (editor-state-message st) "1 change; before"
                  (editor-state-buf st) (restore-snapshot
                                         (editor-state-buf st) snap))))
        (setf (editor-state-message st) "Already at oldest change"))))

(defun do-redo (st)
  (let ((stack (editor-state-redo st)))
    (if stack
        (progn
          (push (snapshot-buffer (editor-state-buf st))
                (editor-state-undo st))
          (let ((snap (pop stack)))
            (setf (editor-state-redo st) stack
                  (editor-state-message st) "1 change; after"
                  (editor-state-buf st) (restore-snapshot
                                         (editor-state-buf st) snap))))
        (setf (editor-state-message st) "Already at newest change"))))

;;; ---------- word/char classification (default 'iskeyword') ----------

(defun char-class (ch)
  "Default vim classes: keyword chars, blank, punctuation."
  (cond
    ((alphanumericp ch) :word)
    ((eql ch #\_) :word)
    ((member ch '(#\Space #\Tab)) :blank)
    (t :punct)))

(defun line-class-at (line col)
  "Class of char at COL, or :newline past end."
  (cond ((>= col (length line)) :newline)
        (t (char-class (char line col)))))

;;; ---------- word motions across the buffer ----------

(defun buffer-nth-line (buf row)
  (nth row (buffer-lines buf)))

(defun last-line-no (buf)
  (1- (length (buffer-lines buf))))

;; forward word start: from ROW/COL to start of next word/punct-run,
;; crossing newlines; empty lines count as a word (motion.txt w)
(defun motion-w (buf row col)
  "vim w: to start of next word. Two phases: step over the current
non-blank run, then skip blanks (spaces and newlines) to the next
word start. Empty lines count as words."
  (let* ((lines (buffer-lines buf))
         (nlines (length lines)))
    ;; phase 1: leave the current run of word/punct chars
    (let ((cls (line-class-at (buffer-nth-line buf row) col)))
      (unless (or (eql cls :newline) (eql cls :blank))
        (loop
          (incf col)
          (let ((c (line-class-at (buffer-nth-line buf row) col)))
            (when (or (eql c :newline) (not (eql c cls)))
              (return))))))
    ;; phase 2: skip blanks to next word start
    (loop
      (let ((c (line-class-at (buffer-nth-line buf row) col)))
        (cond
          ((eql c :blank) (incf col))
          ((eql c :newline)
           (if (= row (1- nlines))
               (return (values row (max 0 (1- (length (buffer-nth-line buf row))))))
               (progn
                 (incf row)
                 (setf col 0)
                 (when (zerop (length (buffer-nth-line buf row)))
                   (return (values row 0))))))
          (t (return (values row col))))))))

;; backward word start
(defun motion-b (buf row col)
  (loop
    (cond
      ((zerop col)
       (cond ((zerop row) (return (values 0 0)))
             (t (decf row)
                (setf col (max 0 (1- (length (buffer-nth-line buf row))))))))
      (t
       (decf col)
       (let ((cls (char-class (char (buffer-nth-line buf row) col))))
         ;; skip spaces back
         (loop while (and (plusp col)
                          (eql (char (buffer-nth-line buf row) (1- col))
                               #\Space))
               do (decf col))
         ;; skip leading space class handled above; if landed mid-word,
         ;; walk to word start
         (let ((c2 (unless (>= col (length (buffer-nth-line buf row)))
                     (char-class (char (buffer-nth-line buf row) col)))))
           (when (and c2 (not (eql c2 :blank)))
             (loop while (and (plusp col)
                              (let ((pc (char (buffer-nth-line buf row) (1- col))))
                                (and (not (eql pc #\Space))
                                     (eql (char-class pc) c2))))
                   do (decf col))))
         (return (values row col)))))))

;; end of word forward (inclusive motion target)
(defun motion-e (buf row col)
  (let* ((line (buffer-nth-line buf row))
         (len (length line)))
    ;; step right first (vim e always advances at least one)
    (when (< col (1- len))
      (incf col))
    (loop
      (let* ((l2 (buffer-nth-line buf row))
             (n2 (length l2)))
        (cond
          ((>= col n2)
           (if (= row (last-line-no buf))
               (return (values row (max 0 (1- n2))))
               (progn (incf row) (setf col 0))))
          ((eql (char l2 col) #\Space)
           (if (< col (1- n2)) (incf col)
               (progn
                 (if (= row (last-line-no buf))
                     (return (values row (1- n2)))
                     (progn (incf row) (setf col 0))))))
          (t
           ;; at non-space: extend to end of its class run
           (let ((cls (char-class (char l2 col))))
             (loop while (and (< (1+ col) (length (buffer-nth-line buf row)))
                              (eql (char-class (char (buffer-nth-line buf row) (1+ col))) cls))
                   do (incf col))
             (return (values row col)))))))))

;;; ---------- generic motion executor ----------
;;; Returns (values end-row end-col inclusive-p linewise-p)
(defun execute-motion (buf key count)
  "Apply motion KEY COUNT times to BUF's cursor conceptually; returns
target coordinates without moving the buffer cursor."
  (let ((row (buffer-row buf)) (col (buffer-col buf))
        (inclusive nil) (linewise nil))
    (dotimes (_ (or count 1))
      (multiple-value-bind (r2 c2)
          (case key
            (#\w (motion-w buf row col))
            (#\b (motion-b buf row col))
            (#\e (setf inclusive t) (motion-e buf row col))
            (#\0 (values row 0))
            (#\^ (values row (max 0 (or (position-if
                                         (lambda (ch) (not (eql ch #\Space)))
                                         (buffer-nth-line buf row)) 0))))
            (#\$ (setf inclusive t)
                 (values row (max 0 (1- (length (buffer-nth-line buf row))))))
            (#\G (setf linewise t) (values (last-line-no buf) 0))
            (#\j (setf linewise t) (values (min (last-line-no buf) (1+ row)) col))
            (#\k (setf linewise t) (values (max 0 (1- row)) col))
            (#\h (values row (max 0 (1- col))))
            (#\l (values row (min (length (buffer-nth-line buf row)) (1+ col))))
            (#\- (setf linewise t)
                 (values (max 0 (1- row))
                         (max 0 (or (position-if (lambda (ch) (not (eql ch #\Space)))
                                                 (buffer-nth-line buf (max 0 (1- row)))) 0))))
            (t (return-from execute-motion (values nil nil nil))))
        (setf row r2 col c2)))
    (values row col inclusive linewise)))

;;; ---------- operator application ----------

(defun delete-range (buf sr sc er ec inclusive)
  "Charwise delete from (SR,SC) through (ER,EC); returns
(values new-buf grabbed-lines)."
  (let ((lines (copy-tree (buffer-lines buf))))
    (if (= sr er)
        (let* ((line (nth sr lines))
               (end (if inclusive (1+ ec) ec))
               (text (subseq line sc end)))
          (setf (nth sr lines)
                (concatenate 'string (subseq line 0 sc) (subseq line end)))
          (values (%make-buffer lines sr
                                (min sc (max 0
                                     (1- (length (nth sr lines)))))
                                (buffer-filename buf))
                  (list text)))
        ;; multiline charwise: splice head+tail, drop middle rows
        (let* ((head (nth sr lines))
               (tail (nth er lines))
               (end (if inclusive (1+ ec) ec))
               (first-text (subseq head sc))
               (mid (subseq lines (1+ sr) er))
               (last-text (subseq tail 0 end))
               (newline (concatenate 'string (subseq head 0 sc)
                                     (subseq tail end)))
               (newlines (append (subseq lines 0 sr)
                                 (cons newline (subseq lines (1+ er))))))
          (values (%make-buffer newlines sr sc (buffer-filename buf))
                  (cons first-text (append mid (list last-text))))))))

(defun yank-range (buf sr sc er ec inclusive)
  (if (= sr er)
      (let* ((line (nth sr (buffer-lines buf)))
             (end (if inclusive (1+ ec) ec)))
        (list (subseq line sc end)))
      (let* ((head (nth sr (buffer-lines buf)))
             (tail (nth er (buffer-lines buf)))
             (end (if inclusive (1+ ec) ec)))
        (cons (subseq head sc)
              (append (subseq (buffer-lines buf) (1+ sr) er)
                      (list (subseq tail 0 end)))))))

(defun put-register (buf reg below-p count)
  "Put register REG (list-of-lines . linewise-p) after (below-p=T)
or before the cursor, per usr_04.5 semantics."
  (unless reg (return-from put-register buf))
  (destructuring-bind (text . linewise-p) reg
    (let ((rep (make-list (max 1 (or count 1)) :initial-element text)))
      (cond
        (linewise-p
         (let* ((row (buffer-row buf))
                (at (if below-p (1+ row) row))
                (newlines (apply #'append
                                 (mapcar (lambda (blk) (copy-tree blk))
                                         rep)))
                (lines (buffer-lines buf)))
           (%make-buffer (append (subseq lines 0 at)
                                 newlines
                                 (subseq lines at))
                         at 0 (buffer-filename buf))))
        (t
         (let* ((joined (apply #'append rep))
                (one-line (reduce (lambda (a b) (concatenate 'string a b)) joined
                                  :initial-value ""))
              (row (buffer-row buf))
              (col (buffer-col buf))
              (line (nth row (buffer-lines buf)))
              (at (min (length line)
                       (if below-p (1+ col) col)))
              (newline (concatenate 'string
                                    (subseq line 0 at) one-line
                                    (subseq line at)))
              (lines (copy-tree (buffer-lines buf))))
           (setf (nth row lines) newline)
           (%make-buffer lines row (+ at -1 (length one-line))
                         (buffer-filename buf))))))))

;;; ---------- helpers used by the normal-mode step ----------

(defun first-non-blank-col (line)
  (max 0 (or (position-if (lambda (ch) (not (eql ch #\Space))) line) 0)))

(defun set-register-charwise (st lines)
  (setf (editor-state-register st) (cons lines nil)))

(defun set-register-linewise (st lines)
  (setf (editor-state-register st) (cons lines t)))

(defun enter-insert (st buf)
  "Record undo point once per insert session, switch mode."
  (push-undo st)
  (setf (editor-state-buf st) buf
        (editor-state-mode st) :insert
        ;; vim replaces any command-line message with -- INSERT --
        (editor-state-message st) nil))

(defun apply-operator (st op sr sc er ec inclusive linewise)
  "Apply d/c/y over the motion range; handles cw-as-ce exception."
  (let ((buf (editor-state-buf st)))
    (if linewise
        (let* ((nlines (length (buffer-lines buf))))
          (case op
            (:delete
             (let* ((grabbed (subseq (buffer-lines buf)
                                     sr (1+ (min er (1- nlines)))))
                    (newlines (append (subseq (buffer-lines buf) 0 sr)
                                      (subseq (buffer-lines buf)
                                              (1+ (min er (1- nlines)))))))
               (set-register-linewise st grabbed)
               (if (null newlines)
                   (progn
                     (setf (editor-state-buf st)
                           (%make-buffer nil 0 0 (buffer-filename buf)))
                     (setf (editor-state-message st)
                           "--No lines in buffer--"))
                   (let ((row (min sr (1- (length newlines)))))
                     (setf (editor-state-buf st)
                           (%make-buffer newlines row
                                         (first-non-blank-col
                                          (nth row newlines))
                                         (buffer-filename buf)))))))
            (:yank
             (set-register-linewise
              st (subseq (buffer-lines buf) sr (1+ (min er (1- nlines))))))
            (:change
             (let ((lines (copy-tree (buffer-lines buf))))
               (setf (nth sr lines) "")
               (set-register-linewise
                st (subseq (buffer-lines buf) sr (1+ (min er (1- nlines)))))
               (setf (editor-state-buf st)
                     (%make-buffer (append (subseq lines 0 sr)
                                           (list "")
                                           (subseq lines (1+ sr)))
                                   sr 0 (buffer-filename buf)))
               (enter-insert st (editor-state-buf st))))))
        ;; charwise range
        (multiple-value-bind (nb grabbed)
            (ecase op
              (:delete (delete-range buf sr sc er ec inclusive))
              (:yank (values buf (yank-range buf sr sc er ec inclusive)))
              (:change (delete-range buf sr sc er ec inclusive)))
          (set-register-charwise st grabbed)
          (setf (editor-state-buf st) nb)
          (if (eq op :change)
              (progn
                (push-undo st)
                (setf (editor-state-mode st) :insert))
              ;; d/y: park cursor at start col clamped to line
              (let ((line (nth sr (buffer-lines nb))))
                (buffer-set-cursor nb sr (min sc (max 0 (1- (length line)))))))))))

;;; ---------- Normal-mode step (the command grammar, usr_04) ----------

(defun update-showcmd (st)
  "Mirror vim's showcmd: count digits then operator char."
  (let ((parts))
    (when (editor-state-op-count st)
      (push (princ-to-string (editor-state-op-count st)) parts))
    (when (editor-state-pending-count st)
      (push (princ-to-string (editor-state-pending-count st)) parts))
    (when (editor-state-pending-op st)
      (push (string (case (editor-state-pending-op st)
                      (:delete #\d) (:change #\c) (:yank #\y)))
            parts))
    (setf (editor-state-showcmd st)
          (if parts (apply #'concatenate 'string (nreverse parts)) nil))))

(defun clear-command-state (st)
  (setf (editor-state-pending-op st) nil
        (editor-state-pending-count st) nil
        (editor-state-op-count st) nil
        (editor-state-pending-arg-for st) nil)
  (update-showcmd st))

(defun find-char-in-line (line from dir kind ch)
  "f/F/t/T engine. DIR=+1 forward, -1 back. KIND=:exact or :till.
Returns column or NIL."
  (let ((len (length line)))
    (labels ((scan-fwd ()
               (loop for i from from below len
                     when (eql (char line i) ch)
                       return (if (eq kind :till) (1- i) i)))
             (scan-back ()
               (loop for i downfrom from above -1
                     when (eql (char line i) ch)
                       return (if (eq kind :till) (1+ i) i))))
      (if (plusp dir) (scan-fwd) (scan-back)))))

(defun replace-chars (buf n ch)
  "r command. CH=NIL means <Enter>: split line at cursor."
  (let* ((line (buffer-current-line buf))
         (col (buffer-col buf))
         (lines (copy-tree (buffer-lines buf))))
    (cond
      ((null ch)
       ;; r<Enter> replaces one char with a line break
       (unless (>= col (length line))
         (let ((head (subseq line 0 col))
               (tail (subseq line (1+ col))))
           (setf (nth (buffer-row buf) lines) head)
           (values (%make-buffer (append (subseq lines 0 (buffer-row buf))
                                         (list head tail)
                                         (subseq lines (1+ (buffer-row buf))))
                                 (1+ (buffer-row buf)) 0
                                 (buffer-filename buf))
                   t))))
      (t
       (unless (>= col (length line))
         (let* ((end (min (+ col (max n 1)) (length line)))
                (rep (make-string (- end col) :initial-element ch))
                (newline (concatenate 'string (subseq line 0 col)
                                      rep (subseq line end))))
           (setf (nth (buffer-row buf) lines) newline)
           ;; vim leaves the cursor on the LAST replaced char
           (values (%make-buffer lines (buffer-row buf)
                                 (max 0 (1- end))
                                 (buffer-filename buf))
                   nil)))))))

(defun delete-chars-forward (buf n)
  "x = dl. Returns (values new-buf grabbed-lines)."
  (let* ((line (buffer-current-line buf))
         (col (buffer-col buf))
         (end (min (+ col n) (length line))))
    (if (< col (length line))
        (delete-range buf (buffer-row buf) col (buffer-row buf) end nil)
        (values nil nil))))

(defun delete-chars-backward (buf n)
  "X = dh"
  (let* ((col (buffer-col buf)))
    (when (plusp col)
      (delete-range buf (buffer-row buf) (max 0 (- col n))
                    (buffer-row buf) (1- col) t))))

(defun to-eol-delete (buf change-p)
  "D=d$ / C=c$"
  (let* ((col (buffer-col buf))
         (len (length (buffer-current-line buf))))
    (if (<= len col)
        buf
        (multiple-value-bind (nb _)
            (delete-range buf (buffer-row buf) col (buffer-row buf)
                          (1- len) t)
          (declare (ignore _))
          nb))))

;;; ---------- Normal-mode dispatcher, decomposed ----------
;;; Grammar: {count}{op}{count}{motion} | {count}{simple-cmd}

(defun norm-char (norm)
  "Character payload of a (:CHAR c) normalization, else NIL."
  (when (and (consp norm) (eq (first norm) :char))
    (second norm)))

(defun count-or-1 (st)
  (or (editor-state-pending-count st) 1))

(defun total-op-count (st)
  (* (or (editor-state-pending-count st) 1)
     (max 1 (or (editor-state-op-count st) 1))))

;;; ----- pending argument completion (r / f / F / t / T) -----

(defun do-replace-arg (st norm ch key)
  "Complete r: replace [count] chars; <Enter> splits the line."
  (let* ((n (count-or-1 st))
         (buf (editor-state-buf st))
         (enter-p (or (eq norm :enter)
                      (and (integerp key) (= key 13))))
         (escape-p (eq norm :escape))
         (arg (cond (escape-p :cancel)
                    (enter-p nil)
                    (t ch))))
    (clear-command-state st)
    (unless (eq arg :cancel)
      (push-undo st)
      (multiple-value-bind (nb _)
          (replace-chars buf n arg)
        (declare (ignore _))
        (when nb (setf (editor-state-buf st) nb))))))

(defun do-find-arg (st kind ch)
  "Complete f/F/t/T: hop to match, repeating [count] times."
  (clear-command-state st)
  (when ch
    (let* ((buf (editor-state-buf st))
           (line (buffer-current-line buf))
           (dir (if (member kind '(:find :till)) 1 -1))
           (kind2 (case kind (:find-back :exact) (:till-back :exact)
                        (:find :exact) (:till :till)))
           (target (buffer-col buf)))
      (dotimes (_ (count-or-1 st))
        (let* ((from (if (plusp dir) (1+ target) (1- target)))
               (hit (find-char-in-line line from dir kind2 ch)))
          (if hit (setf target hit) (return))))
      (unless (= target (buffer-col buf))
        (setf (editor-state-buf st)
              (buffer-set-cursor buf (buffer-row buf) target))))))

(defun pending-arg-step (st key norm ch)
  "Feed awaited argument of r/f/F/t/T. Returns handled-p."
  (let ((what (editor-state-pending-arg-for st)))
    (cond ((eq what :replace) (do-replace-arg st norm ch key) t)
          ((member what '(:find :find-back :till :till-back))
           (do-find-arg st what ch) t)
          (t nil))))

;;; ----- operator completion -----

(defun linewise-operator (st op)
  "dd / cc / yy with composed counts."
  (let* ((buf (editor-state-buf st))
         (row (buffer-row buf))
         (n (min (total-op-count st)
                 (- (length (buffer-lines buf)) row))))
    (push-undo st)
    (apply-operator st op row 0 (+ row n -1) 0 nil t)
    (clear-command-state st)))

(defun walk-motion-endpoint (buf mkey total)
  "Apply motion MKEY on a probe copy TOTAL times, chaining each hop.
Returns (values er ec inclusive linewise ok)."
  (let ((probe (copy-buffer buf))
        hit-row hit-col hit-inc hit-lw)
    (dotimes (_ total)
      (multiple-value-bind (r2 c2 inc lw)
          (execute-motion probe mkey 1)
        (when (null r2)
          (return-from walk-motion-endpoint
            (values nil nil nil nil nil)))
        (setf hit-row r2 hit-col c2 hit-inc inc hit-lw lw)
        (let ((clamped-row (min r2 (last-line-no probe))))
          (setf probe
                (%make-buffer (buffer-lines probe)
                              clamped-row
                              (min c2 (length (buffer-nth-line
                                               probe clamped-row)))
                              (buffer-filename probe))))))
    (values hit-row hit-col hit-inc hit-lw t)))

(defun motion-operator (st op mkey)
  "Complete d/c/y over a motion range. Handles cw-as-ce."
  (let* ((buf (editor-state-buf st))
         (mkey2 (if (cw-as-ce-p buf op mkey) #\e mkey))
         (total (total-op-count st)))
    (clear-command-state st)
    (multiple-value-bind (er ec inclusive linewise ok)
        (walk-motion-endpoint buf mkey2 total)
      (when ok
        (push-undo st)
        (apply-operator st op (buffer-row buf) (buffer-col buf)
                        er ec inclusive
                        (or linewise (member mkey '(#\G #\j #\k))))))))

(defun cw-as-ce-p (buf op mkey)
  "vim exception: cw behaves like ce when starting on non-blank."
  (and (eq op :change) (eql mkey #\w)
       (let ((col (buffer-col buf)) (line (buffer-current-line buf)))
         (and (< col (length line))
              (not (eql (char line col) #\Space))))))

(defun accumulate-op-count (st ch)
  (setf (editor-state-op-count st)
        (+ (* 10 (or (editor-state-op-count st) 0))
           (- (char-code ch) 48)))
  (update-showcmd st))

(defun operator-step (st norm ch)
  "Second key while an operator is pending. Always consumes."
  (let ((op (editor-state-pending-op st)))
    (cond ((operator-doubled-p op ch) (linewise-operator st op)
           (update-showcmd st))
          ((and ch (digit-char-p ch) (not (eql ch #\0)))
           (accumulate-op-count st ch))
          ((motion-key-p ch) (motion-operator st op ch))
          (t (clear-command-state st)))
    t))

(defun operator-doubled-p (op ch)
  (and ch (case op
            (:delete (eql ch #\d))
            (:change (eql ch #\c))
            (:yank   (eql ch #\y)))))

(defun motion-key-p (ch)
  (and ch (member ch '(#\w #\b #\e #\$ #\0 #\G #\j #\k))))

;;; ----- simple commands -----

(defun motion-command (st ch n)
  "Plain cursor motions with counts."
  (let ((buf (editor-state-buf st))
        (new-buf nil))
    (case ch
      (#\h (setf new-buf (buffer-move buf 0 (- n))))
      (#\l (setf new-buf (buffer-move buf 0 n)))
      (#\j (setf new-buf (vertical-move buf n)))
      (#\k (setf new-buf (vertical-move buf (- n))))
      (#\w (setf new-buf (move-by-motions buf #'motion-w n)))
      (#\b (setf new-buf (move-by-motions buf #'motion-b n)))
      (#\e (setf new-buf (move-by-motions buf #'motion-e n)))
      (#\0 (setf new-buf (buffer-set-cursor buf (buffer-row buf) 0)))
      (#\^ (setf new-buf
                 (buffer-set-cursor buf (buffer-row buf)
                                    (first-non-blank-col
                                     (buffer-current-line buf)))))
      (#\$ (setf new-buf
                 ;; land on last char, curswant to infinity: chained
                 ;; j/k stick to end-of-line like real vim
                 (buffer-set-cursor buf (buffer-row buf)
                                    (max 0 (1- (length
                                                (buffer-current-line buf))))
                                    9999)))
      (#\G (let ((row (if (= n 1) (last-line-no buf) (min (1- n)
                                                          (last-line-no buf)))))
             (setf new-buf (buffer-set-cursor buf row 0)))))
    (when new-buf
      (clear-command-state st)
      (setf (editor-state-buf st) new-buf)
      t)))

(defun edit-shortcut (st ch n)
  "x X D C s S — quick delete/change forms."
  (let* ((buf (editor-state-buf st))
         (nb nil))
    (case ch
      (#\x (push-undo st)
           (multiple-value-bind (b2 grabbed)
               (delete-chars-forward buf n)
             (setf nb b2)
             (when b2 (setf (editor-state-register st)
                            (cons grabbed nil)))))
      (#\X (push-undo st) (setf nb (delete-chars-backward buf n)))
      (#\D (push-undo st) (setf nb (to-eol-delete buf nil)))
      (#\C (progn (push-undo st)
                  (setf nb (to-eol-delete buf nil))))
      (#\s (push-undo st)
           (multiple-value-bind (b2 _) (delete-chars-forward buf n)
             (declare (ignore _)) (setf nb b2)))
      (#\S (push-undo st)
           (apply-operator st :change (buffer-row buf) 0
                          (+ (buffer-row buf) (1- n)) 0 nil t)))
    (when nb (setf (editor-state-buf st) nb))
    (unless (member ch '(#\C #\s))
      (clear-command-state st))
    ;; C and s drop into insert mode
    (when (member ch '(#\C #\s))
      (enter-insert st (editor-state-buf st)))
    t))

(defun yank-put-undo-key (st ch key)
  "Y p P u CTRL-R."
  (let ((buf (editor-state-buf st))
        (n (count-or-1 st)))
    (case ch
      (#\Y (set-register-linewise st (list (buffer-current-line buf)))
           (clear-command-state st))
      (#\p (push-undo st)
           (setf (editor-state-buf st)
                 (put-register buf (editor-state-register st) t n))
           (clear-command-state st))
      (#\P (push-undo st)
           (setf (editor-state-buf st)
                 (put-register buf (editor-state-register st) nil n))
           (clear-command-state st))
      (#\u (do-undo st) (clear-command-state st))
      (t   (when (and (integerp key) (= key 18))   ; CTRL-R
             (do-redo st) (clear-command-state st))))))

(defun misc-key (st ch)
  "~ J :"
  (case ch
    (#\: (clear-command-state st)
         (setf (editor-state-mode st) :ex
               (editor-state-ex-acc st) "")
         t)
    (t nil)))

(defun insert-entry (st ch)
  "i I a A o O."
  (let ((buf (editor-state-buf st)))
    (case ch
      (#\i (enter-insert st buf))
      (#\I (setf (editor-state-buf st)
                 (buffer-set-cursor buf (buffer-row buf)
                                    (first-non-blank-col
                                     (buffer-current-line buf))))
           (enter-insert st (editor-state-buf st)))
      (#\a (setf (editor-state-buf st) (buffer-move buf 0 1))
           (enter-insert st (editor-state-buf st)))
      (#\A (setf (editor-state-buf st)
                 (buffer-set-cursor buf (buffer-row buf)
                                    (length (buffer-current-line buf))))
           (enter-insert st (editor-state-buf st)))
      (#\o (enter-insert st (buffer-open-line-below buf)))
      (#\O (enter-insert st (buffer-open-line-above buf))))
    t))

(defun toggle-case-at (line i)
  (let ((c (char line i)))
    (if (both-case-p c)
        (if (upper-case-p c) (char-downcase c) (char-upcase c))
        c)))

(defun tilde-command (st n)
  "~ swaps case and advances, vim-style."
  (let* ((buf (editor-state-buf st))
         (col (buffer-col buf))
         (line (buffer-current-line buf))
         (end (min (+ col n) (length line))))
    (when (< col (length line))
      (push-undo st)
      (let ((new (copy-seq line)))
        (loop for i from col below end
              do (setf new
                       (concatenate 'string
                                    (subseq new 0 i)
                                    (string (toggle-case-at new i))
                                    (subseq new (1+ i)))))
        (let ((lines (copy-tree (buffer-lines buf))))
          (setf (nth (buffer-row buf) lines) new)
          (setf (editor-state-buf st)
                (%make-buffer
                 lines (buffer-row buf)
                 (min end (max 0 (1- (length new))))
                 (buffer-filename buf))))))))

(defun join-command (st n)
  (push-undo st)
  (join-lines st n))

(defun simple-command-step (st key norm ch)
  "Non-operator commands. Returns handled-p."
  (cond ((and ch (member ch '(#\h #\l #\j #\k #\w #\b #\e
                              #\0 #\^ #\$ #\G)))
         (motion-command st ch (count-or-1 st)))
        ((and ch (member ch '(#\x #\X #\D #\C #\s #\S)))
         (edit-shortcut st ch (count-or-1 st)) t)
        ((and ch (member ch '(#\Y #\p #\P #\u)))
         (yank-put-undo-key st ch key) t)
        ((and (integerp key) (= key 18))
         (do-redo st) (clear-command-state st) t)
        ((eq norm :escape) (clear-command-state st) t)
        ((and ch (member ch '(#\i #\I #\a #\A #\o #\O)))
         (clear-command-state st)
         (insert-entry st ch))
        ((eql ch #\~)
         (clear-command-state st)
         (tilde-command st (count-or-1 st)) t)
        ((eql ch #\J)
         (clear-command-state st)
         (join-command st (count-or-1 st)) t)
        ((eql ch #\:)
         (misc-key st ch))
        ;; operator initiators
        ((and ch (member ch '(#\d #\c #\y)))
         (setf (editor-state-pending-op st)
               (case ch (#\d :delete) (#\c :change) (#\y :yank)))
         (update-showcmd st) t)
        ;; f/F/t/T/r argument initiators
        ((and ch (member ch '(#\f #\F #\t #\T)))
         (let ((n (editor-state-pending-count st)))
           (clear-command-state st)
           (setf (editor-state-pending-count st) n)
           (setf (editor-state-pending-arg-for st)
                 (case ch (#\f :find) (#\F :find-back)
                       (#\t :till) (#\T :till-back)))
           (setf (editor-state-showcmd st)
                 (format nil "~@[~a~]~a"
                         (if (> (or n 1) 1) n nil) ch))
           t))
        ((eql ch #\r)
         (let ((n (count-or-1 st)))
           (clear-command-state st)
           (setf (editor-state-pending-count st) n)
           (setf (editor-state-pending-arg-for st) :replace)
           (setf (editor-state-showcmd st)
                 (format nil "~@[~a~]r" (if (> n 1) n nil)))
           t))
        ;; arrows keep working in normal mode
        (t (case norm
             (:left (setf (editor-state-buf st)
                          (buffer-move (editor-state-buf st) 0 -1)) t)
             (:right (setf (editor-state-buf st)
                           (buffer-move (editor-state-buf st) 0 1)) t)
             (:up (setf (editor-state-buf st)
                        (vertical-move (editor-state-buf st) -1)) t)
             (:down (setf (editor-state-buf st)
                          (vertical-move (editor-state-buf st) 1)) t)
             (t nil)))))

(defun count-prefix-step (st ch)
  "Leading digits start a count; lone 0 is a motion."
  (when (and ch (digit-char-p ch) (not (eql ch #\0)))
    (setf (editor-state-pending-count st)
          (+ (* 10 (or (editor-state-pending-count st) 0))
             (- (char-code ch) 48)))
    (update-showcmd st)
    t))

(defun normal-step (st key)
  "One Normal-mode keystroke against editor-state ST."
  (let* ((norm (normalize-key key))
         (ch (norm-char norm)))
    (or (pending-arg-step st key norm ch)
        (when (editor-state-pending-op st)
          (operator-step st norm ch))
        (count-prefix-step st ch)
        (simple-command-step st key norm ch))
    ;; vim clears the showcmd field once nothing is pending
    (unless (or (editor-state-pending-op st)
                (editor-state-pending-count st)
                (editor-state-pending-arg-for st))
      (setf (editor-state-showcmd st) nil))))

(defun move-by-motions (buf fn n)
  "Apply word-motion FN (buf -> values row col) N times."
  (let ((row (buffer-row buf)) (col (buffer-col buf)))
    (dotimes (_ (max 1 n) (buffer-set-cursor buf row col))
      (multiple-value-bind (r c) (funcall fn buf row col)
        (setf row r col c)))))

;;; ---------- support for normal-step ----------

(defun vertical-move (buf dr)
  "j/k with curswant semantics (buffer-move handles want-col)."
  (buffer-move buf dr 0))

(defun join-lines (st n)
  "J joins [count] lines (default 2)."
  (let* ((buf (editor-state-buf st))
         (times (max 1 (1- n)))
         (lines (copy-tree (buffer-lines buf))))
    (dotimes (_ times)
      (let* ((row (buffer-row buf))
             (next-row (1+ row)))
        (when (< next-row (length lines))
          (let* ((cur (nth row lines))
                 (nxt (nth next-row lines))
                 (cur-trimmed (string-right-trim '(#\Space #\Tab) cur))
                 (sep (if (or (string= nxt "")
                              (/= (length cur-trimmed) (length cur))
                              (and (plusp (length cur))
                                   (eql (char cur (1- (length cur))) #\Space)))
                          ""
                          " "))
                 (joined (concatenate 'string cur-trimmed sep nxt)))
            (setf lines (append (subseq lines 0 row)
                                (list joined)
                                (subseq lines (1+ next-row))))
            (setf buf (%make-buffer
                       lines row
                       (length cur-trimmed)
                       (buffer-filename buf)
                       (length cur-trimmed)))))))
    (setf (editor-state-buf st) buf)))

(defun insert-step (st key)
  "Insert-mode keystroke through the state machine."
  (multiple-value-bind (nb nm msg)
      (editor-key-insert (editor-state-buf st) key)
    (setf (editor-state-buf st) nb
          (editor-state-mode st) nm
          (editor-state-message st) msg)))
