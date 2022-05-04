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

(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

(setq inhibit-startup-message t)

(blink-cursor-mode -1)
(mouse-avoidance-mode 'jump)
(setq x-stretch-cursor t)
