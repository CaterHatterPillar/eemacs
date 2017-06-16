(setq load-path (cons "~/.emacs.d" load-path))

(require 'linenum)
(require 'local nil t)
(require 'midnight)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(require 'whitespace)
(setq whitespace-style '(empty face lines-tail tabs trailing))
(global-whitespace-mode t)

(desktop-save-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(mouse-avoidance-mode 'jump)
(global-set-key "\C-x\C-b" 'buffer-menu)

(setq column-number-mode t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq font-lock-global-modes '(not test-log-mode))
(setq ido-default-buffer-method 'selected-window)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq x-stretch-cursor t)
(setq-default indent-tabs-mode nil)

(setq scroll-step            1
      scroll-conservatively  10000)

(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

(if (version< emacs-version "24.4")
    (global-set-key (kbd "M-SPC") 'just-one-space)
    (global-set-key (kbd "M-SPC") 'cycle-spacing))

;; Show full filename in frame title bar:
(setq frame-title-format
     	'((:eval (if (buffer-file-name)
      		(abbreviate-file-name (buffer-file-name))
      		"%b"))))

;; Auto-indent after newline:
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))
             
(defun prompt-error-unless-visiting-buffer ()
  (unless buffer-file-name (error "Buffer not visiting a file")))

(defun copy-file-name ()
  "Copy the current buffer file name."
  (interactive)
  (prompt-error-unless-visiting-buffer)
  (kill-new (file-name-nondirectory
	   (buffer-file-name
	    (window-buffer
	     (minibuffer-selected-window))))))

(defun copy-file-directory ()
  "Copy the current buffer directory."
  (interactive)
  (prompt-error-unless-visiting-buffer)
  (kill-new (file-name-directory
	   (buffer-file-name
	    (window-buffer
	     (minibuffer-selected-window))))))

(defun copy-file-path ()
  "Copy the current buffer full path."
  (interactive)
  (prompt-error-unless-visiting-buffer)
  (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; Candidates
;; ----------

;; If on your MAC, do this:
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

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
