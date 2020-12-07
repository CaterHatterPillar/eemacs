(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages '(elpy
                   flycheck
                   py-autopep8
                   rjsx-mode
                   xref-js2
                   stgit))
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      packages)

(require 'miscfun)

(require 'local nil t)  ; in case there's a machine-specific configuration

;;; APPEARANCE

(let ((theme-path "~/.emacs.d/themes/emacs-color-theme-solarized")
      (load-custom-theme (lambda ()
                           (add-to-list 'custom-theme-load-path theme-path)
                           (load-theme 'solarized t)
                           (set-frame-parameter nil 'background-mode 'dark)
                           (enable-theme 'solarized))))
  (funcall load-custom-theme))

(setq frame-title-format  ; Full filename in titlebar
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(menu-bar-mode -1)
(when (display-graphic-p)
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

(when (version<= "26.0.50" emacs-version )
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

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

(add-hook 'prog-mode-hook 'show-paren-mode)

(setq-default indent-tabs-mode nil)
(setq initial-scratch-message nil)

(setq grep-scroll-output t)

(add-hook 'c-mode-hook
          '(lambda () (define-key c-mode-map "\C-m" 'newline-and-indent)))

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

;; Use elpy to run tests, but use emacs-tdd to automatically run them
(add-to-list 'load-path "~/.emacs.d/emacs-tdd")
(require 'tdd)
(add-hook 'python-mode-hook 'tdd-mode)
(pyvenv-activate "/home/pcadmin/anaconda3/envs/mimer")

;; Set these to enable conda in the compilation environment
;; TODO: this fucks up some bash commands in Emacs, like find-name-dired
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-ic")

(setq gud-pdb-command-name "python -m pdb")

;;; JAVASCRIPT

(setq indent-tabs-mode nil
      js-indent-level 2)

(eval-after-load 'rjsx-mode  ; Don't complete HTML brackets
  '(progn
     (define-key rjsx-mode-map "<" nil)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))  ; rjsx-mode as default

(add-hook 'rjsx-mode-hook (lambda () (add-hook 'xref-backend-functions
                                               #'xref-js2-xref-backend nil t)))

(require 'xref-js2)

(require 'prettier-js)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;;; WINDOWS

(winner-mode 1)
(windmove-default-keybindings)

;;; MISC

(defalias 'yes-or-no-p 'y-or-n-p)
(setq font-lock-global-modes '(not test-log-mode))
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill t)
(setq echo-keystrokes 0.01)

(defun is-linux ()
  (string-equal system-type "gnu/linux"))
(when (is-linux)
  (progn (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
         (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
           (add-hook hook (lambda () (flyspell-mode -1))))
         (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))))

;;; ALIASES

(defalias 'dl 'delete-matching-lines)
(defalias 'kl 'keep-lines)
(defalias 'sl 'sort-lines)

;;; BINDINGS

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-,") 'previous-error)
(global-set-key (kbd "C-.") 'next-error)
(global-set-key (kbd "C-<tab>") 'hippie-expand)

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'kill-compilation)
(global-set-key (kbd "<f9>") 'lazy-guess-args)

(if (version< emacs-version "24.4")
    (global-set-key (kbd "M-SPC") 'just-one-space)
    (global-set-key (kbd "M-SPC") 'cycle-spacing))

;;; CANDIDATES

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pylint stgit prettier-js prettier yafolding indium conda flycheck elpy anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
