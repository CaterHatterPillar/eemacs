;; GUI
;; ---

;; Use a custom dark theme (color-scheme Solarized):
;; Clone from https://github.com/sellout/emacs-color-theme-solarized.git
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

(tool-bar-mode -1) ;; Hide the toolbar graphic.
(menu-bar-mode -1) ;; Hide the menu bar.
(toggle-scroll-bar -1) ;; Hide scroll bar at the right-hand side of windows.
(blink-cursor-mode -1) ;; A flashing cursor is distracting. Disable it.
(setq inhibit-startup-message t) ;; Hide Emacs welcome screen.

(show-paren-mode 1) ;; Highlight matching parentheses.
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs.

;; Working with Emacs
;; ------------------

;; Auto-indent after newline:
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))
             
;; Let emacs scroll at one line at a time:
(setq scroll-step            1
      scroll-conservatively  10000)

(global-linum-mode t) ;; Display line numbers on the left-hand side of buffers.
(defadvice forward-paragraph (after forward-paragraph-linum-update)
  "Perform (linum-update-current) after jumping forward one
  paragraph to ensure line numbers are being rendered
  correctly."
  (global-linum-mode t)
  (linum-update-current))
(ad-activate 'forward-paragraph)

;; Buffers
;; -------

;; Improve naming of buffers with the same name inside of Emacs:
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(global-auto-revert-mode 1) ;; Automatically reload a file if modified outside of Emacs.

;; Schedule the closing of buffers (default action) deemed unused at midnight:
(require 'midnight)

;; Candidates
;; ----------

;; If on your MAC, do this:
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Something about the highlighting of whitespace:
(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)
;; Don't highlight empty lines (or something like that; I forgot):
(setq whitespace-trailing-regexp
  "\\b\\(\\(\t\\| \\|\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\)+\\)$")
  
(defun astyle (pmin pmax)
(interactive "r")
(shell-command-on-region pmin pmax
             "C:/AStyle/bin/astyle.exe --style=google --unpad-paren --add-brackets --indent=spaces=4 --close-templates"
             (current-buffer) t
             (get-buffer-create "*Astyle Errors*") t))
             
;; Some functions will split your window. These variable changes
;; ensure that splits occur vertically (side-by-side windows) rather
;; than horizontally:
(setq split-height-threshold nil)
(setq split-width-threshold 0)
