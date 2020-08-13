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

(defun lazy-break-args ()
  (interactive)
  (let ((tokens (split-string (thing-at-point 'line t) ", ")))
    (unless (= (length tokens) 1)
      (let* ((breaks (mapcar (lambda (p) (format "%s,\n" p)) (butlast tokens)))
             (lines (append breaks (last tokens))))
        (move-beginning-of-line nil)
        (kill-line 1)
        (mapc (lambda (l) (insert l) (indent-according-to-mode)) lines)))))

(defun cur-line ()
  (+ (count-lines 1 (point)) 1))

(defun end-char-at-line (&optional line)
  (unless line
    (setq line (cur-line)))
  (save-excursion (forward-line (- line (cur-line)))
                  (move-end-of-line nil)
                  (char-before)))

(defun find-last-line-ending-with (char next-fun back-fun max)
  (while (and (equal (end-char-at-line) char)
              (/= (cur-line) max))
    (funcall next-fun))
  (unless (equal (end-char-at-line) char)
    (funcall back-fun))  ;; Doesn't make sense at the end of a buffer
  (cur-line))

(defun backwards-last-line-ending-with (char)
  (find-last-line-ending-with char
                              (lambda () (forward-line -1))
                              (lambda () (forward-line))
                              1))

(defun forwards-last-line-ending-with (char)
  (find-last-line-ending-with char
                              (lambda () (forward-line))
                              (lambda () (forward-line -1))
                              (line-number-at-pos (point-max))))

(defun lazy-unbreak-args ()
  (interactive)
  (let ((pre-line-end (end-char-at-line (- (cur-line) 1)))
        (cur-line-end (end-char-at-line)))
    (when (or (eq pre-line-end ?,)
              (eq cur-line-end ?,))
      (unless (equal cur-line-end ?,)
        (forward-line -1))
      (let* ((beg-line (backwards-last-line-ending-with ?,))
             (end-line (+ (forwards-last-line-ending-with ?,) 1))
             (num-join (- end-line beg-line)))
        (forward-line (- end-line (cur-line)))
        (dotimes (i num-join) (delete-indentation))))))

(defun lazy-guess-args ()
  (interactive)
  (if (= (length (split-string (thing-at-point 'line t) ", ")) 1)
      (lazy-unbreak-args)
    (lazy-break-args)))

(defun rotate-windows ()
  (interactive)
  (when (= (count-windows) 2)
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1))))

(defun is-linux ()
  (string-equal system-type "gnu/linux"))

(defun dos2unix ()
  "Remove all carriage returns"
  (interactive)
  (save-excursion (goto-char (point-min))
                  (while (search-forward "\r" nil t)
                    (replace-match ""))))

(defun unix2dos ()
  "Add a carriage return to every line feed"
  (interactive)
  (save-excursion (goto-char (point-min))
                  (while (search-forward "\n" nil t)
                    (replace-match "\r\n"))))

(defun apply-prejudice ()
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(provide 'miscfun)
