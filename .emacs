;; Use a custom dark theme (color-scheme Solarized).
;; Clone from https://github.com/sellout/emacs-color-theme-solarized.git
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

(blink-cursor-mode -1) ;; A flashing cursor is distracting. Disable it.

 ;; Automatically reload a file if modified outside of Emacs:
(global-auto-revert-mode 1)

;; Customize the naming of buffers somehow. I forgot the purpose of this:
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

;; Schedule the closing of buffers (default action) deemed unused at midnight:
(require 'midnight)

(show-paren-mode 1) ;; Highlight matching parentheses.

(setq inhibit-startup-message t) ;; Hide Emacs welcome screen.

(global-linum-mode t)

(defadvice forward-paragraph (after forward-paragraph-linum-update)
  "Perform (linum-update-current) after jumping forward one
  paragraph to ensure line numbers are being rendered
  correctly."
  (global-linum-mode t)
  (linum-update-current))
(ad-activate 'forward-paragraph)

;; If on your MAC, do this:
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Auto-indent after newline:
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))

;; Something about the highlighting of whitespace:
(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)
;; Don't highlight empty lines (or something like that; I forgot):
(setq whitespace-trailing-regexp
  "\\b\\(\\(\t\\| \\|\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\)+\\)$")
  
;; Use spaces instead of tabs:
(setq-default indent-tabs-mode nil)

(tool-bar-mode -1) ;; Hide toolbar graphics.
(menu-bar-mode -1) ;; Hide toolbar.
(toggle-scroll-bar -1) ;; Hide scroll bar at the right-hand side of windows.

;; Let emacs scroll at one line at a time:
(setq scroll-step            1
      scroll-conservatively  10000)
