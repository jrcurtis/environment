
(add-to-list 'load-path "~/.emacs.d")

(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :font "dejavu sans mono-12")

(tool-bar-mode 0)

(load "color-theme-sunburst.el")
(color-theme-tm)

(require 'web-mode)
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(rassq-delete-all 'html-helper-mode magic-mode-alist)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq-default scss-sass-command "compass compile #")

(load "aj-compilation.el")

(unload-feature 'aquamacs-tabbar t)
(unload-feature 'tabbar t)



