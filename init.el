(setq load-path (cons "~/.emacs.d" load-path))

(require 'appearance)
(require 'keybind)
(require 'linenum)
(require 'local nil t)
(require 'miscfun)
(require 'template)

(require 'midnight)

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

;; Candidates
;; ----------

;; If on your MAC, do this:
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
