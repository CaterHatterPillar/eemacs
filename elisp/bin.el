;; No idea why there's a line-number-at-pos but no...
(defun column-number-at-pos (pos)
  (save-excursion (goto-char pos) (current-column)))

(defun reset-x ()
  (setq l -1
        r -1
        x -1))

(defun bin-middle-x ()
  (let ((column (setq x (/ (+ l r) 2))))
    (if (= column (current-column))
        (reset-x)
      (move-to-column column))))

(defun bin-forward ()
  (if (= x (current-column))
      (setq l x)
    (reset-x))
  (when (< r 0)
    (setq l (current-column)
          r (column-number-at-pos (line-end-position))))
  (bin-middle-x))

(defun bin-back ()
  (if (= x (current-column))
      (setq r x)
    (reset-x))
  (when (< l 0)
    (setq l (column-number-at-pos (line-beginning-position))
          r (current-column)))
  (bin-middle-x))

(defun reset-y ()
  (setq u -1
        d -1
        y -1))

;; It'd be useful to remember the 'original' column so that, when a
;; row jump sequence traverses shorter lines on the way, subsequent
;; jumps aren't offset to the left.
(defun bin-middle-y ()
  (let ((col (current-column))
        (row (setq y (/ (+ u d) 2))))
    (if (= row (line-number-at-pos))
        (reset-y)
      (progn (goto-line row)
             (move-to-column col)))))

(defun bin-up ()
  (if (= y (line-number-at-pos))
      (setq d y)
    (reset-y))
  (when (< u 0)
    (setq u (line-number-at-pos (window-start))
          d (line-number-at-pos)))
  (bin-middle-y))

(defun bin-down ()
  (if (= y (line-number-at-pos))
      (setq u y)
    (reset-y))
  (when (< d 0)
    (setq u (line-number-at-pos)
          d (line-number-at-pos (window-end))))
  (bin-middle-y))

(defun twice-unless-moved (lunge)
  (let ((past-point (point)))
    (funcall lunge)
    (when (= (point) (past-point))
      (funcall lunge))))

(defun lunge-forward ()
  (interactive)
  (twice-unless-moved 'bin-forward))

(defun lunge-back ()
  (interactive)
  (twice-unless-moved 'bin-back))

(defun lunge-up ()
  (interactive)
  (twice-unless-moved 'bin-up))

(defun lunge-down ()
  (interactive)
  (twice-unless-moved 'bin-down))

(reset-x)
(reset-y)

;; (global-set-key (kbd "M-<right>") 'lunge-forward)
;; (global-set-key (kbd "M-<left>") 'lunge-back)

;; (global-set-key (kbd "M-<up>") 'lunge-up)
;; (global-set-key (kbd "M-<down>") 'lunge-down)
