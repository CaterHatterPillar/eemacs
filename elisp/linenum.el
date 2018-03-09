(add-hook 'prog-mode-hook 'linum-mode)

(unless (version< emacs-version "24")
  (defadvice forward-paragraph (after forward-paragraph-linum-update)
    "Perform (linum-update-current) after jumping forward one
     paragraph to ensure line numbers are being rendered
     correctly."
    (linum-update-current))
  (ad-activate 'forward-paragraph))

(provide 'linenum)
