(require 'whitespace)
(setq whitespace-style '(empty face lines-tail tabs trailing))
(global-whitespace-mode t)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-decoration-mode
                                  global-semantic-highlight-func-mode
                                  global-semantic-stickyfunc-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-idle-breadcrumbs-mode
                                  global-semantic-mru-bookmark-mode))

(add-hook 'prog-mode-hook 'semantic-mode)

(add-hook 'prog-mode-hook 'show-paren-mode)

(setq-default indent-tabs-mode nil)
(setq initial-scratch-message nil)

(setq grep-scroll-output t)

(add-hook 'c-mode-hook
          '(lambda () (define-key c-mode-map "\C-m" 'newline-and-indent)))

(defun notify-compilation-complete(buffer msg)
  (if (string-match "^finished" msg)
      (tooltip-show "\n Compilation Success \n ")
    (tooltip-show "\n Compilation Failure \n ")))

(when (display-graphic-p)
  (add-to-list 'compilation-finish-functions 'notify-compilation-complete))

(setq compilation-scroll-output 'first-error)

(defun display-piped-output (buffer msg)
  (unless (string-match "^finished" msg)
    (switch-to-buffer "test.log")))

(define-minor-mode moscow-compilation-mode
  "Expect compilation output piped to file"
  :global t
  (if moscow-compilation-mode
      (add-hook 'compilation-finish-functions 'display-piped-output)
    (remove-hook 'compilation-finish-functions 'display-piped-output)))

(require 'diff)
(defun delete-trailing-whitespace-at-modified-lines ()
  (when buffer-file-name
    (when (derived-mode-p 'prog-mode)
      (save-excursion
        (with-current-buffer
            (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
          (diff-delete-trailing-whitespace)
          (kill-buffer))))))

(when (eq system-type 'gnu/linux)
  (add-hook 'before-save-hook 'delete-trailing-whitespace-at-modified-lines))

(provide 'programming)
