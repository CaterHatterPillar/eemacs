(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(add-hook 'python-mode-hook
          '(lambda () (highlight-indentation-mode 0)))

(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

(pyvenv-activate "~/anaconda3/envs/tensorflow")

(provide 'custom-python)
