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
