(setq load-path (cons "~/.emacs.d/elisp" load-path))

(require 'aliases)
(require 'appearance)
(require 'keybind)
(require 'linenum)
(require 'local nil t)
(require 'miscfun)
(require 'numbers)
(require 'programming)
(require 'template)

(require 'midnight)

;; State

(setq savehist-file "~/.emacs.d/history")
(setq savehist-additional-variables
      '(compile-history kill-ring regexp-search-ring search-ring))
(savehist-mode 1)

(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Buffers

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(setq auto-revert-interval 1)
(global-auto-revert-mode 1)

(setq confirm-nonexistent-file-or-buffer nil)

(setq scroll-step            1
      scroll-conservatively  10000)

;; Windows

(winner-mode 1)
(windmove-default-keybindings)

;; Misc

(defalias 'yes-or-no-p 'y-or-n-p)
(setq font-lock-global-modes '(not test-log-mode))
(setq ring-bell-function 'ignore)

(when (is-linux)
  (progn (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
         (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
           (add-hook hook (lambda () (flyspell-mode -1))))
         (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))))

;; Candidates
;; ----------

;; If on your MAC, do this:
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
