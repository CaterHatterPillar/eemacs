(require 'cl)

(defun pred (pid name)
  (let ((user (user-uid))
        (attributes (process-attributes pid)))
    (and (eq (assoc 'euid attributes) user)
         (eq (assoc 'comm attributes) name))))

(defun matching-pids (name)
  (let ((pred (lambda (pid) (funcall #'pred pid name)))
        (pids (list-system-processes)))
    (remove-if-not pred pids)))

(defun debug-program (name)
  "Attempt to attach gdb to a process matching name"
  (interactive "sName: ")
  (let* ((pids (matching-pids name))
         (num_pids (length pids)))
    (cond ((eq num_pids 0) (error "No matching process found"))
          ((eq num_pids 1) (gdb (format "gdb --pid %d" (car pids))))
          (t (error "Multiple matches found")))))

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

(provide 'miscfun)
