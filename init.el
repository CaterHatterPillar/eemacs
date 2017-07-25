(setq load-path (cons "~/.emacs.d" load-path))

(require 'appearance)
(require 'linenum)
(require 'local nil t)

(require 'midnight)

(require 'on-screen)
(on-screen-global-mode +1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(require 'whitespace)
(setq whitespace-style '(empty face lines-tail tabs trailing))
(global-whitespace-mode t)

(setq savehist-file "~/.emacs.d/history")
(setq savehist-additional-variables
      '(compile-history kill-ring regexp-search-ring search-ring))
(savehist-mode 1)

(global-auto-revert-mode 1)
(show-paren-mode 1)
(winner-mode 1)

(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-stickyfunc-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-breadcrumbs-mode
        global-semantic-mru-bookmark-mode))
(add-hook 'prog-mode-hook 'semantic-mode)

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-,") 'previous-error)
(global-set-key (kbd "C-.") 'next-error)

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'recompile)

(windmove-default-keybindings)

(setq confirm-nonexistent-file-or-buffer nil)
(setq font-lock-global-modes '(not test-log-mode))
(setq ido-default-buffer-method 'selected-window)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)

(setq scroll-step            1
      scroll-conservatively  10000)

(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(if (version< emacs-version "24.4")
    (global-set-key (kbd "M-SPC") 'just-one-space)
    (global-set-key (kbd "M-SPC") 'cycle-spacing))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Show full filename in frame title bar:
(setq frame-title-format
     	'((:eval (if (buffer-file-name)
      		(abbreviate-file-name (buffer-file-name))
      		"%b"))))

;; Auto-indent after newline:
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))
             
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

;; Candidates
;; ----------

;; If on your MAC, do this:
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
