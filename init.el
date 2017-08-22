(setq load-path (cons "~/.emacs.d" load-path))

(require 'appearance)
(require 'keybind)
(require 'linenum)
(require 'local nil t)
(require 'miscfun)
(require 'numbers)
(require 'programming)
(require 'template)

(require 'midnight)

;; ELPA/MELPA

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'c-mode-hook #'aggressive-indent-mode)

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

(setq font-lock-global-modes '(not test-log-mode))
(setq ido-default-buffer-method 'selected-window)
(setq ring-bell-function 'ignore)

(defalias 'dl 'delete-matching-lines)
(defalias 'kl 'keep-lines)
(defalias 'sl 'sort-lines)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Candidates
;; ----------

;; If on your MAC, do this:
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
