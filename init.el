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
  ;; (setq pyvenv-workon "emacs")  ; fallback, doesn't work with lsp for whatever reason
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-tracking-mode 1)  ; use add-dir-local-variable to set 'pyenv-workon'
  (pyvenv-mode t))

(use-package lsp-mode
  :ensure t
  :after pyvenv
  :hook
  ((python-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  ;;       (:map lsp-mode-map
  ;;        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

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

(setq split-height-threshold 999)

(use-package python-mode  ; built-in doesn't support 3.10 match cases
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :init
  (defconst polarium-protobuf-style
    '((c-basic-offset . 8)))
  (add-hook 'protobuf-mode-hook
	    (lambda () (c-add-style "polarium-protobuf-style" polarium-protobuf-style t))))
