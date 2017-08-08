(require 'whitespace)
(setq whitespace-style '(empty face lines-tail tabs trailing))
(global-whitespace-mode t)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-decoration-mode
                                  global-semantic-highlight-func-mode
                                  global-semantic-stickyfunc-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-idle-breadcrumbs-mode
                                  global-semantic-mru-bookmark-mode))
(add-hook 'prog-mode-hook 'semantic-mode)

(add-hook 'prog-mode-hook 'show-paren-mode)

(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-hook
          '(lambda () (define-key c-mode-map "\C-m" 'newline-and-indent)))

(defun notify-compilation-complete(buffer msg)
  (if (string-match "^finished" msg)
      (tooltip-show "\n Compilation Success \n ")
    (tooltip-show "\n Compilation Failure \n ")))
(add-to-list 'compilation-finish-functions 'notify-compilation-complete)
(setq compilation-scroll-output 'first-error)

(provide 'programming)
