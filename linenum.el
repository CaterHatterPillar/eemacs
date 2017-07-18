(global-linum-mode t)

(defadvice forward-paragraph (after forward-paragraph-linum-update)
  "Perform (linum-update-current) after jumping forward one
  paragraph to ensure line numbers are being rendered
  correctly."
  (global-linum-mode t)
  (linum-update-current))
(ad-activate 'forward-paragraph)

(provide 'linenum)
