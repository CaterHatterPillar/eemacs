(require 'cl)

(defun system-processes-qualifying-predicate (pred)
  (let ((pids (list-system-processes)))
    (remove-if-not pred pids)))

(defun uid-and-name (pid regex)
  (let ((user (user-uid))
        (attributes (process-attributes pid)))
    (and (eq (cdr (assoc 'euid attributes)) user)
         (string-match regex (cdr (assoc 'comm attributes))))))

(defun debug-program (name pid)
  "Attempt to attach gdb to a process matching name"
  (interactive
   (let* ((name (read-string "Name (regex): "))
          (pred (lambda (pid) (funcall #'uid-and-name pid name)))
          (pids (mapcar 'number-to-string
                        (system-processes-qualifying-predicate pred)))
          (num_pids (length pids)))
     (cond ((eq num_pids 0) (error "No matching process found"))
           ((eq num_pids 1) (list name (car pids)))
           ((minibuffer-with-setup-hook (lambda () (funcall #'proced t))
              (let* ((prompt "Multiple matches. Select PID: ")
                     (pid (completing-read prompt pids nil t)))
                (list name (string-to-number pid))))))))
  (gdb (format "gdb --pid %d" pid)))

(defun current-file-name ()
  (unless buffer-file-name (error "Buffer not visiting a file"))
  (buffer-file-name (window-buffer (minibuffer-selected-window))))

(defun copy-file-name ()
  "Copy the current buffer file name."
  (interactive)
  (kill-new (file-name-nondirectory (current-file-name))))

(defun copy-file-directory ()
  "Copy the current buffer directory."
  (interactive)
  (kill-new (file-name-directory (current-file-name))))

(defun copy-file-path ()
  "Copy the current buffer full path."
  (interactive)
  (kill-new (current-file-name)))

(defun reverse-at-point (thing)
  (let ((bounds (bounds-of-thing-at-point thing))
        (at-point (thing-at-point thing)))
    (delete-region (car bounds) (cdr bounds))
    (insert (reverse at-point))))

(defun reverse-word ()
  (interactive)
  (reverse-at-point 'word))

(defun reverse-symbol ()
  (interactive)
  (reverse-at-point 'symbol))

(defun rename-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless buffer-file-name (error "Buffer not visiting a file"))
    (let ((new-filename (read-file-name "Rename to: " filename)))
      (if (vc-backend filename)
          (vc-rename-file filename new-filename)
        (rename-file filename new-filename t))
      (set-visited-file-name new-filename)
      (set-buffer-modified-p nil))))

(defun remove-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless buffer-file-name (error "Buffer not visiting a file"))
    (if (vc-backend filename)
        (vc-delete-file filename)
      (delete-file filename))
    (kill-buffer)))

(defun bounds-of-binary-at-point ()
  (save-excursion
    (skip-chars-backward "b01")
    (if (looking-at "0b[01]+")
        (cons (point) (match-end 0))
      nil)))

(defun bounds-of-octal-at-point ()
  (save-excursion
    (skip-chars-backward "01234567")
    (if (looking-at "0[0-7]+")
        (cons (point) (match-end 0))
      nil)))

(defun bounds-of-decimal-at-point ()
  (save-excursion
    (skip-chars-backward "-0123456789")
    (if (looking-at "-?[0-9]+")
        (cons (point) (match-end 0))
      nil)))

(defun bounds-of-hex-at-point ()
  (save-excursion
    (skip-chars-backward "x0123456789abcdefABCDEF")
    (if (looking-at "0x[0-9a-fA-F]+")
        (cons (point) (match-end 0))
      nil)))

(put 'binary 'bounds-of-thing-at-point 'bounds-of-binary-at-point)
(put 'octal 'bounds-of-thing-at-point 'bounds-of-octal-at-point)
(put 'decimal 'bounds-of-thing-at-point 'bounds-of-decimal-at-point)
(put 'hex 'bounds-of-thing-at-point 'bounds-of-hex-at-point)

(defun format-binary (b)
  (let ((s ""))
    (while (> b 0)
      (if (logand b 1)
          (setq s (concat "1" s))
        (setq s (concat "0" s)))
      (setq b (lsh b -1)))
    (message s)))

(defun radix-at-point ()
  (cond ((thing-at-point 'binary) 'binary)
        ((thing-at-point 'octal) 'octal)
        ((thing-at-point 'hex) 'hex)
        ((thing-at-point 'decimal) 'decimal)
        (t (error "NaN at point"))))

(defun elastic-number-at-point ()
  (let* ((number (radix-at-point))
         (notation-end 0)
         (radix 10))
    (cond ((eq number 'binary)
           (setq notation-end 2)
           (setq radix 2))
          ((eq number 'octal)
           (setq notation-end 1)
           (setq radix 8))
          ((eq number 'hex)
           (setq notation-end 2)
           (setq radix 16)))
    (string-to-number
     (substring (thing-at-point number) notation-end nil) radix)))

(defun format-number-at-point (format-string)
  (let ((at-point (elastic-number-at-point)))
    (if (equal format-string "%b")
        (message (format-binary at-point))
      (format format-string at-point))))

(defun as-binary ()
  (interactive)
  (let ((binary (concat "0b" (format-number-at-point "%b"))))
    (if (called-interactively-p 'interactive)
        (message binary)
      binary)))

(defun as-octal ()
  (interactive)
  (let ((octal (concat "0" (format-number-at-point "%o"))))
    (if (called-interactively-p 'interactive)
        (message octal)
      octal)))

(defun as-decimal ()
  (interactive)
  (let ((decimal (format-number-at-point "%d")))
    (if (called-interactively-p 'interactive)
        (message decimal)
      decimal)))

(defun as-hex ()
  (interactive)
  (let ((hex (concat "0x" (format-number-at-point "%x"))))
    (if (called-interactively-p 'interactive)
        (message hex)
      hex)))

(defun convert-number-at-point (as)
  (let ((bounds (bounds-of-thing-at-point (radix-at-point))))
    (delete-region (car bounds) (cdr bounds)))
  (insert as))

(defun to-binary ()
  (interactive)
  (convert-number-at-point (as-binary)))

(defun to-octal ()
  (interactive)
  (convert-number-at-point (as-octal)))

(defun to-decimal ()
  (interactive)
  (convert-number-at-point (as-decimal)))
 
(defun to-hex ()
  (interactive)
  (convert-number-at-point (as-hex)))

(provide 'miscfun)
