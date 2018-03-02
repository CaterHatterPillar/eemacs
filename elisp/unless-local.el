(when (eq system-type 'gnu/linux)  ; no diff on windows
  (require 'diff)
  (defun delete-trailing-whitespace-at-modified-lines ()
    (when (and buffer-file-name (derived-mode-p 'prog-mode))
      (save-excursion
        (with-current-buffer
            (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
          (diff-delete-trailing-whitespace)
          (kill-buffer)))))

  (add-hook 'before-save-hook 'delete-trailing-whitespace-at-modified-lines))

(provide 'unless-local)
