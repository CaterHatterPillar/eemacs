(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package no-littering
  :ensure t
  :config
  (setq backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq recentf-save-file (no-littering-expand-var-file-name "recentf"))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package midnight
  :ensure t)

(use-package stgit
  :ensure t)

(use-package magit
  :ensure t)

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (setq pyvenv-workon "emacs")  ; fallback
  (pyvenv-tracking-mode 1))  ; use add-dir-local-variable to set 'pyenv-workon'

(use-package lsp-mode
  :ensure t
  :hook
  ((python-mode . lsp)))

(use-package lsp-ui
  :ensure t)

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package undo-tree  ; TODO: consider vundo
  :ensure t)

(use-package zoom
  :ensure t
  :config
  (zoom-mode))

(use-package expand-region
  :ensure t
  :config
  :bind (("C-=" . er/expand-region)))

(use-package restart-emacs
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (setq beacon-blink-when-window-scrolls nil)
  (setq beacon-blink-when-point-moves nil)
  (beacon-mode 1))

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package rg
  :ensure t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(show-paren-mode 1)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'jump)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-auto-revert-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq column-number-mode t)
(setq x-stretch-cursor t)
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Prevent trailing whitespace in modified lines
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

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-.") 'next-error)
(global-set-key (kbd "C-,") 'previous-error)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
