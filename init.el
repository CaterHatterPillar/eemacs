(setq load-path (cons "~/.emacs.d" load-path))

(require 'linenum)
(require 'local nil t)

;; GUI
;; ---

;; Use a custom dark theme (color-scheme Solarized):
;; Clone from https://github.com/sellout/emacs-color-theme-solarized.git
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

(tool-bar-mode -1) ;; Hide the toolbar graphic.
(menu-bar-mode -1) ;; Hide the menu bar.
(toggle-scroll-bar -1) ;; Hide scroll bar at the right-hand side of windows.
(blink-cursor-mode -1) ;; A flashing cursor is distracting. Disable it.
(setq inhibit-startup-message t) ;; Hide Emacs welcome screen.

(show-paren-mode 1) ;; Highlight matching parentheses.
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs.

;; Show full filename in frame title bar:
(setq frame-title-format
     	'((:eval (if (buffer-file-name)
      		(abbreviate-file-name (buffer-file-name))
      		"%b"))))

;; Working with Emacs
;; ------------------

;; Auto-indent after newline:
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))
             
;; Let emacs scroll at one line at a time:
(setq scroll-step            1
      scroll-conservatively  10000)

;; Buffers
;; -------

;; Improve naming of buffers with the same name inside of Emacs:
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(global-auto-revert-mode 1) ;; Automatically reload a file if modified outside of Emacs.

;; Schedule the closing of buffers (default action) deemed unused at midnight:
(require 'midnight)

;; Don't prompt if buffer does not exist at switch:
(setq confirm-nonexistent-file-or-buffer nil)

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

(require 'cl)

(defun kill-other-buffers ()
 "Kill all other buffers."
 (interactive)
 (mapc 'kill-buffer 
  (delq (current-buffer) 
   (remove-if-not 'buffer-file-name (buffer-list)))))

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; check out https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;

(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 

;; Consider configuring the desktop mode.
;; (desktop-save-mode 1) ;; Open previously open buffers when starting Emacs.

;; ...but don't allow IDO to automatically focus in on other frames if
;; opening the same buffer in another frame.
(setq ido-default-buffer-method 'selected-window)

(setq column-number-mode t) ;; Show column numbers (in bottom bar).

;; Avoid fontifying the Simics test.log buffer
(setq font-lock-global-modes '(not test-log-mode))
