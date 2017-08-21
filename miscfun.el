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

(defun simple-bounds-at-point (allowed-chars forward-regex)
  (save-excursion
    (skip-chars-backward allowed-chars)
    (if (looking-at forward-regex)
        (cons (point) (match-end 0))
      nil)))

(defun bounds-of-binary-at-point ()
  (simple-bounds-at-point "b01" "0b[01]+"))

(defun bounds-of-octal-at-point ()
  (simple-bounds-at-point "01234567" "0[0-7]+"))

(defun bounds-of-decimal-at-point ()
  (simple-bounds-at-point "-0123456789" "-?[0-9]+"))

(defun bounds-of-hex-at-point ()
  (simple-bounds-at-point "x0123456789abcdefABCDEF" "0x[0-9a-fA-F]+"))

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

(defun numeric-radix (radix)
  (cond ((eq radix 'binary) 2)
        ((eq radix 'octal) 8)
        ((eq radix 'decimal) 10)
        ((eq radix 'hex) 16)))

(defun radix-prefix (radix)
  (cond ((eq radix 'binary) "0b")
        ((eq radix 'octal) "0")
        ((eq radix 'decimal) "")
        ((eq radix 'hex) "0x")))

(defun radix-format (radix)
  (cond ((eq radix 'octal) "%o")
        ((eq radix 'decimal) "%d")
        ((eq radix 'hex) "%x")))

(defun number-at-point ()
  (let* ((radix (radix-at-point))
         (skip-prefix (length (radix-prefix radix)))
         (numeric-radix (numeric-radix radix)))
    (string-to-number (substring (thing-at-point radix) skip-prefix nil)
                      numeric-radix)))

(defun format-number-as (number radix)
  (let ((prefix (radix-prefix radix)))
    (if (eq radix 'binary)
        (concat prefix (format-binary number))
      (concat prefix (format (radix-format radix) number)))))

(defun at-point-as (radix)
  (format-number-as (number-at-point) radix))

(defun at-point-as-binary ()
  (at-point-as 'binary))

(defun at-point-as-octal ()
  (at-point-as 'octal))

(defun at-point-as-decimal ()
  (at-point-as 'decimal))

(defun at-point-as-hex ()
  (at-point-as 'hex))

(defun as-binary ()
  (interactive)
  (message (at-point-as-binary)))

(defun as-octal ()
  (interactive)
  (message (at-point-as-octal)))

(defun as-decimal ()
  (interactive)
  (message (at-point-as-decimal)))

(defun as-hex ()
  (interactive)
  (message (at-point-as-hex)))

(defun convert-number-at-point (as)
  (let ((bounds (bounds-of-thing-at-point (radix-at-point))))
    (delete-region (car bounds) (cdr bounds)))
  (insert as))

(defun to-binary ()
  (interactive)
  (convert-number-at-point (at-point-as-binary)))

(defun to-octal ()
  (interactive)
  (convert-number-at-point (at-point-as-octal)))

(defun to-decimal ()
  (interactive)
  (convert-number-at-point (at-point-as-decimal)))

(defun to-hex ()
  (interactive)
  (convert-number-at-point (at-point-as-hex)))

(provide 'miscfun)
