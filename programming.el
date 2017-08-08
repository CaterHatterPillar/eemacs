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

(setq compilation-scroll-output 'first-error)
(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-hook
          '(lambda () (define-key c-mode-map "\C-m" 'newline-and-indent)))

(provide 'programming)
