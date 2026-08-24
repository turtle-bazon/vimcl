(defpackage #:vimcl/tests
  (:use #:cl #:fiveam #:vimcl))

(in-package #:vimcl/tests)

(def-suite* :vimcl
  :description "vimcl tests")

;;; --- buffer core ---

(test buffer-empty
  (let ((buf (make-empty-buffer)))
    (is (= 1 (buffer-line-count buf)))
    (is (string= "" (buffer-current-line buf)))
    (is (= 0 (buffer-row buf)))
    (is (= 0 (buffer-col buf)))))

(test buffer-insert-char
  (let ((buf (make-empty-buffer)))
    (is (string= "h" (buffer-current-line (buffer-insert-char buf #\h))))
    ;; insert advances the cursor
    (let ((b2 (buffer-insert-char buf #\h)))
      (is (= 0 (buffer-row b2)))
      (is (= 1 (buffer-col b2))))))

(test buffer-insert-mid-line
  (let ((buf (make-empty-buffer)))
    (loop for ch across "abc" do (setf buf (buffer-insert-char buf ch)))
    (is (string= "axbc" (buffer-current-line (buffer-insert-char (buffer-move buf 0 -2) #\x))))))



(test buffer-delete-char
  (let ((buf (make-empty-buffer)))
    (loop for ch across "abc" do (setf buf (buffer-insert-char buf ch)))
    (is (string= "ac" (buffer-current-line (buffer-delete-char (buffer-move buf 0 -2)))))))



(test buffer-delete-backward-joins-lines
  (let ((buf (make-empty-buffer)))
    (loop for ch across "abcd" do (setf buf (buffer-insert-char buf ch)))
    (setf buf (buffer-move buf 0 -2))
    (setf buf (buffer-split-line buf))
    (let ((b2 (buffer-delete-backward buf)))
      (is (= 1 (buffer-line-count b2)))
      (is (string= "abcd" (buffer-current-line b2)))
      (is (= 0 (buffer-row b2)))
      (is (= 2 (buffer-col b2))))))



(test buffer-delete-backward-at-origin
  (let ((buf (make-empty-buffer)))
    (is (eq buf (buffer-delete-backward buf)))))

(test buffer-split-line
  (let ((buf (make-empty-buffer)))
    (loop for ch across "abcd" do (setf buf (buffer-insert-char buf ch)))
    (setf buf (buffer-move buf 0 -2))
    (let ((b2 (buffer-split-line buf)))
      (is (= 2 (buffer-line-count b2)))
      (is (string= "ab" (nth 0 (buffer-lines b2))))
      (is (string= "cd" (nth 1 (buffer-lines b2))))
      (is (= 1 (buffer-row b2)))
      (is (= 0 (buffer-col b2))))))



(test buffer-move-clamped
  (let ((buf (make-empty-buffer)))
    (loop for ch across "abc" do (setf buf (buffer-insert-char buf ch)))
    (setf buf (buffer-split-line buf))
    (is (= 0 (buffer-col (buffer-move buf 0 5))))
    (let ((b2 (buffer-move buf -1 0)))
      (is (= 0 (buffer-row b2)))
      (is (= 0 (buffer-col b2))))
    (let ((b3 (buffer-move buf -5 -5)))
      (is (= 0 (buffer-row b3)))
      (is (= 0 (buffer-col b3))))))



(test buffer-purity
  "Operations must not mutate the input buffer."
  (let ((buf (make-empty-buffer)))
    (loop for ch across "ab" do (setf buf (buffer-insert-char buf ch)))
    (buffer-insert-char buf #\x)
    (buffer-delete-char buf)
    (buffer-split-line buf)
    (is (string= "ab" (buffer-current-line buf)))
    (is (= 2 (buffer-col buf)))))


