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

;; Aliases, bindings, and functions are better organized in separate files
(require 'aliases)
(require 'keybind)
(require 'miscfun)

(require 'numbers)
(require 'template)

;;; APPEARANCE

(let ((theme-path "~/.emacs.d/themes/emacs-color-theme-solarized")
      (load-custom-theme (lambda ()
                           (add-to-list 'custom-theme-load-path theme-path)
                           (load-theme 'solarized t)
                           (set-frame-parameter nil 'background-mode 'dark)
                           (enable-theme 'solarized))))
  (unless (or (version< emacs-version "24") (not (display-graphic-p)))
      (funcall load-custom-theme)))

(setq frame-title-format  ; Full filename in titlebar
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(when (display-graphic-p)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

(setq inhibit-startup-message t)

(setq column-number-mode t)

(blink-cursor-mode -1)
(mouse-avoidance-mode 'jump)
(setq x-stretch-cursor t)

;;; HISTORY & BACKUP

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

;;; BUFFERS

(require 'midnight)  ; Clean up buffers after a while

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(setq auto-revert-interval 1)
(global-auto-revert-mode 1)

(setq auto-revert-verbose nil)
(setq confirm-nonexistent-file-or-buffer nil)

(setq scroll-step            1
      scroll-conservatively  10000)

(add-hook 'prog-mode-hook 'linum-mode)

(unless (version< emacs-version "24")
  (defadvice forward-paragraph (after forward-paragraph-linum-update)
    "Perform (linum-update-current) after jumping forward one
     paragraph to ensure line numbers are being rendered
     correctly."
    (linum-update-current))
  (ad-activate 'forward-paragraph))

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

;; Prefer vertical splits on my home system
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;;; PROGRAMMING

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

(when (display-graphic-p)
  (add-to-list 'compilation-finish-functions 'notify-compilation-complete))

(defun notify-compilation-complete(buffer msg)
  (if (string-match "^finished" msg)
      (tooltip-show "\n Compilation Success \n ")
    (tooltip-show "\n Compilation Failure \n ")))

(setq compilation-scroll-output 'first-error)

;; Load stgit after loading a custom theme
(add-to-list 'load-path "/usr/share/stgit/contrib")  ; TODO
(require 'stgit)

;;; PYTHON

(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(add-hook 'python-mode-hook
          '(lambda () (highlight-indentation-mode 0)))  ; Looks terrible

(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

(pyvenv-activate "~/anaconda3/envs/tensorflow")  ; TODO

;; Set these to enable conda in the compilation environment
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;;; WINDOWS

(winner-mode 1)
(windmove-default-keybindings)

;;; MISC

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

;;; CANDIDATES

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

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
