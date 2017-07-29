(defun prompt-error-unless-visiting-buffer ()
  (unless buffer-file-name (error "Buffer not visiting a file")))

(defun copy-file-name ()
  "Copy the current buffer file name."
  (interactive)
  (prompt-error-unless-visiting-buffer)
  (kill-new (file-name-nondirectory
           (buffer-file-name
            (window-buffer
             (minibuffer-selected-window))))))

(defun copy-file-directory ()
  "Copy the current buffer directory."
  (interactive)
  (prompt-error-unless-visiting-buffer)
  (kill-new (file-name-directory
           (buffer-file-name
            (window-buffer
             (minibuffer-selected-window))))))

(defun copy-file-path ()
  "Copy the current buffer full path."
  (interactive)
  (prompt-error-unless-visiting-buffer)
  (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(provide 'miscfun)
