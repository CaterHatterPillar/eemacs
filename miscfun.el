(require 'cl)

(defun system-processes-qualifying-predicate (pred)
  (let ((pids (list-system-processes)))
    (remove-if-not pred pids)))

(defun uid-and-name (pid regex)
  (let ((user (user-uid))
        (attributes (process-attributes pid)))
    (and (eq (cdr (assoc 'euid attributes)) user)
         (string-match regex (cdr (assoc 'comm attributes))))))

(defun debug-program (name pid)
  "Attempt to attach gdb to a process matching name"
  (interactive
   (let* ((name (read-string "Name (regex): "))
          (pred (lambda (pid) (funcall #'uid-and-name pid name)))
          (pids (mapcar 'number-to-string
                        (system-processes-qualifying-predicate pred)))
          (num_pids (length pids)))
     (cond ((eq num_pids 0) (error "No matching process found"))
           ((eq num_pids 1) (list name (car pids)))
           ((minibuffer-with-setup-hook (lambda () (funcall #'proced t))
              (let* ((prompt "Multiple matches. Select PID: ")
                     (pid (completing-read prompt pids nil t)))
                (list name (string-to-number pid))))))))
  (gdb (format "gdb --pid %d" pid)))

(defun current-file-name ()
  (unless buffer-file-name (error "Buffer not visiting a file"))
  (buffer-file-name (window-buffer (minibuffer-selected-window))))

(defun copy-file-name ()
  "Copy the current buffer file name."
  (interactive)
  (kill-new (file-name-nondirectory (current-file-name))))

(defun copy-file-directory ()
  "Copy the current buffer directory."
  (interactive)
  (kill-new (file-name-directory (current-file-name))))

(defun copy-file-path ()
  "Copy the current buffer full path."
  (interactive)
  (kill-new (current-file-name)))

(defun reverse-at-point (thing)
  (let ((bounds (bounds-of-thing-at-point thing))
        (at-point (thing-at-point thing)))
    (delete-region (car bounds) (cdr bounds))
    (insert (reverse at-point))))

(defun reverse-word ()
  (interactive)
  (reverse-at-point 'word))

(defun reverse-symbol ()
  (interactive)
  (reverse-at-point 'symbol))

(defun rename ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless buffer-file-name (error "Buffer not visiting a file"))
    (let ((new-filename (read-file-name "Rename to: " filename)))
      (if (vc-backend filename)
          (vc-rename-file filename new-filename)
        (rename-file filename new-filename t))
      (set-visited-file-name new-filename)
      (set-buffer-modified-p nil))))

(defun format-binary (b)
  (let ((s ""))
    (while (> b 0)
      (if (logand b 1)
          (setq s (concat "1" s))
        (setq s (concat "0" s)))
      (setq b (lsh b -1)))
    (message s)))
 
(defun display-number-at-point (format-string)
  (let ((at-point (thing-at-point 'number)))
    (if (equal format-string "%b")
        (message (format-binary at-point))
      (message (format format-string at-point)))))

(defun as-binary ()
  (interactive)
  (display-number-at-point "%b"))
 
(defun as-octal ()
  (interactive)
  (display-number-at-point "%o"))
 
(defun as-hexadecimal ()
  (interactive)
  (display-number-at-point "%x"))

(defun convert-number-at-point (new)
  ;; bounds-of-thing-at-point doesn't accept 'number
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (delete-region (car bounds) (cdr bounds))
    (insert new)))

(defun to-binary ()
  (interactive)
  (convert-number-at-point (as-binary)))

(defun to-octal ()
  (interactive)
  (convert-number-at-point (as-octal)))
 
(defun to-hexadecimal ()
  (interactive)
  (convert-number-at-point (as-hexadecimal)))

(provide 'miscfun)
