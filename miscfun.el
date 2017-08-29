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

(defun rename-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless buffer-file-name (error "Buffer not visiting a file"))
    (let ((new-filename (read-file-name "Rename to: " filename)))
      (if (vc-backend filename)
          (vc-rename-file filename new-filename)
        (rename-file filename new-filename t))
      (set-visited-file-name new-filename)
      (set-buffer-modified-p nil))))

(defun remove-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless buffer-file-name (error "Buffer not visiting a file"))
    (if (vc-backend filename)
        (vc-delete-file filename)
      (delete-file filename))
    (kill-buffer)))

(defun non-user-buffer (buffer)
  (let ((name (buffer-name buffer)))
    (when (or (string-prefix-p " " name)
              (and (string-prefix-p "*" name) (string-suffix-p "*" name)))
        t)))

(defun num-user-buffers ()
  (length (cl-remove-if 'non-user-buffer (buffer-list))))

;; Provided recently launched...
(defun launched-with-buffer ()
  (when (> (num-user-buffers) 0) t))

(define-minor-mode disk-io-mode
  :global t
  (if disk-io-mode
      (progn (global-auto-revert-mode -1)
             (setq make-backup-files nil)
             (setq auto-save-default nil))
    (progn (global-auto-revert-mode t)
           (setq make-backup-files t)
           (setq auto-save-default t))))

(provide 'miscfun)
