(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

(blink-cursor-mode -1)
(setq x-stretch-cursor t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq column-number-mode t)
(setq inhibit-startup-message t)

(provide 'appearance)
