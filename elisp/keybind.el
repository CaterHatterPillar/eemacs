(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-,") 'previous-error)
(global-set-key (kbd "C-.") 'next-error)
(global-set-key (kbd "C-<tab>") 'hippie-expand)

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'kill-compilation)
(global-set-key (kbd "<f9>") 'lazy-guess-args)

(if (version< emacs-version "24.4")
    (global-set-key (kbd "M-SPC") 'just-one-space)
    (global-set-key (kbd "M-SPC") 'cycle-spacing))

(provide 'keybind)
