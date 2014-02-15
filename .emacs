
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "C:/Users/jacob/.emacs.d/site-lisp")

(show-paren-mode 1)
(setq-default inhibit-splash-screen t)
(set-face-attribute 'default nil :font "dejavu sans mono-12")
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default column-number-mode t)
(setq-default fill-column 80)
(setq-default doc-view-continuous t)
(tool-bar-mode 0)

(global-hl-line-mode 1)
(set-face-background 'highlight "#121")
(set-face-foreground 'highlight nil)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-<tab>") 'other-window)

(require 'color-theme)
(color-theme-initialize)
(load "color-theme-sunburst.el")
(color-theme-tm)

(require 'smooth-scroll)
(smooth-scroll-mode t)
(global-set-key (kbd "C-S-n") #'(lambda ()
                                  (interactive)
                                  (next-line)
                                  (scroll-up-1)))
(global-set-key (kbd "C-S-p") #'(lambda ()
                                  (interactive)
                                  (previous-line)
                                  (scroll-down-1)))

(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-basic-offset . 4)
    (c-offsets-alist        . ((substatement-open . 0)
                               (substatement-close . 0)
                               (inline-open . 0)
                               (comment-intro . 0)
                               (cpp-macro . 0)
                               (case-label . +)
                               (objc-method-call-cont . +)
                               (innamespace . 0)))
    (c-echo-syntactic-information-p . nil))
  "My cc-mode customization for actionscript etc.")
(c-add-style "my-c-style" my-c-style)
(setq c-default-style "my-c-style")

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))

(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt" . cmake-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

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

(when (boundp 'aquamacs-version)
  (unload-feature 'aquamacs-tabbar t)
  (unload-feature 'tabbar t))



