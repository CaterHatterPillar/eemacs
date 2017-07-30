(defun load-custom-theme ()
  (add-to-list 'custom-theme-load-path
               "~/.emacs.d/themes/emacs-color-theme-solarized")
  (load-theme 'solarized t)
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized))

(unless (version< emacs-version "24")
  (load-custom-theme))

;; Full filename in titlebar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(blink-cursor-mode -1)
(mouse-avoidance-mode 'jump)
(setq x-stretch-cursor t)

(when (display-graphic-p)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

(setq inhibit-startup-message t)

(setq column-number-mode t)

(provide 'appearance)
