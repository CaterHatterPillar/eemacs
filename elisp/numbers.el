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
  (simple-bounds-at-point "0123456789" "[0-9]+"))

(defun bounds-of-hex-at-point ()
  (simple-bounds-at-point "x0123456789abcdefABCDEF" "0x[0-9a-fA-F]+"))

(put 'binary 'bounds-of-thing-at-point 'bounds-of-binary-at-point)
(put 'octal 'bounds-of-thing-at-point 'bounds-of-octal-at-point)
(put 'decimal 'bounds-of-thing-at-point 'bounds-of-decimal-at-point)
(put 'hex 'bounds-of-thing-at-point 'bounds-of-hex-at-point)

(defun format-binary (b)
  (let ((s ""))
    (while (> b 0)
      (setq s (concat (number-to-string (logand b 1)) s))
      (setq b (lsh b -1)))
    (if (string= "" s) "0" s)))

(unless (version< emacs-version "24")
  (ert-deftest format-binary ()
	       (should (equal (format-binary 0) "0"))
	       (should (equal (format-binary 1) "1"))
	       (should (equal (format-binary 2) "10"))
	       (should (equal (format-binary 3) "11"))
	       (should (equal (format-binary 4) "100"))))

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

(unless (version< emacs-version "24")
  (ert-deftest format-decimal-as ()
	       (should (equal (format-number-as 255 'binary) "0b11111111"))
	       (should (equal (format-number-as 255 'decimal) "255"))
	       (should (equal (format-number-as 255 'octal) "0377"))
	       (should (equal (format-number-as 255 'hex) "0xff")))

  (ert-deftest format-hex-as ()
	       (should (equal (format-number-as 0xff 'binary) "0b11111111"))
	       (should (equal (format-number-as 0xff 'decimal) "255"))
	       (should (equal (format-number-as 0xff 'octal) "0377"))
	       (should (equal (format-number-as 0xff 'hex) "0xff"))))

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
  "Display a binary, octal, decimal, or hexadecimal at point as binary"
  (interactive)
  (message (at-point-as-binary)))

(defun as-octal ()
  "Display a binary, octal, decimal, or hexadecimal at point as octal"
  (interactive)
  (message (at-point-as-octal)))

(defun as-decimal ()
  "Display a binary, octal, decimal, or hexadecimal at point as decimal"
  (interactive)
  (message (at-point-as-decimal)))

(defun as-hex ()
  "Display a binary, octal, decimal, or hexadecimal at point as hexadecimal"
  (interactive)
  (message (at-point-as-hex)))

(defun convert-number-at-point (as)
  (let ((bounds (bounds-of-thing-at-point (radix-at-point))))
    (delete-region (car bounds) (cdr bounds)))
  (insert as))

(defun to-binary ()
  "Convert a binary, octal, decimal, or hexadecimal at point to binary"
  (interactive)
  (convert-number-at-point (at-point-as-binary)))

(defun to-octal ()
  "Convert a binary, octal, decimal, or hexadecimal at point to octal"
  (interactive)
  (convert-number-at-point (at-point-as-octal)))

(defun to-decimal ()
  "Convert a binary, octal, decimal, or hexadecimal at point to decimal"
  (interactive)
  (convert-number-at-point (at-point-as-decimal)))

(defun to-hex ()
  "Convert a binary, octal, decimal, or hexadecimal at point to hexadecimal"
  (interactive)
  (convert-number-at-point (at-point-as-hex)))

(provide 'numbers)
