;; Configure MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install dependencies
(defvar packages '(elpy
                   flycheck
                   py-autopep8))
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      packages)

(setq load-path (cons "~/.emacs.d/elisp" load-path))

(require 'aliases)
(require 'appearance)
(require 'keybind)
(require 'linenum)
(require 'miscfun)
(require 'numbers)
(require 'programming)
(require 'template)
(require 'custom-python)

(require 'local nil t)
(unless (featurep 'local)
  (require 'unless-local))

(require 'midnight)  ; Clean up buffers after a while

(add-to-list 'load-path "/usr/share/stgit/contrib")  ; TODO
(require 'stgit)

;; Set these so that you can call conda in the compilation environment
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

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

(setq auto-revert-verbose nil)
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
(setq save-interprogram-paste-before-kill t)
(setq echo-keystrokes 0.01)

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

;; Prefer vertical splits on my home system
(setq split-height-threshold nil)
(setq split-width-threshold 160)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (conda flycheck elpy anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
