(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package midnight
  :ensure t)

(use-package stgit
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

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package undo-tree
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

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-mini))
  :config
  (helm-mode 1)
  (setq helm-split-window-in-side-p t))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(show-paren-mode 1)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'jump)
(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq column-number-mode t)
(setq x-stretch-cursor t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

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
