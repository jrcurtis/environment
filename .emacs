
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
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(tool-bar-mode 0)

(defun escape-string (str)
  (if (stringp str)
      (replace-in-string "\"" "\\\"" str)
    ""))

(defun wrap-string (wrapper str)
  (if (listp wrapper)
      (concat (first wrapper) str (last wrapper))
    (concat wrapper str wrapper)))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-<tab>") 'other-window)

(require 'color-theme)
(color-theme-initialize)
(load "color-theme-sunburst.el")
(color-theme-tm)

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

(require 'org)
(setq org-log-done t)
(setq org-use-fast-todo-selection t)
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/jacob.org")
(setq org-agenda-files (list "~/Dropbox/org/jacob.org"))
(setq org-archive-location "~/Dropbox/org/jacob_archive.org")
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(load-file (concat org-directory "/helpers/org.el"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq-default scss-sass-command "compass compile .. #")

(load "aj-compilation.el")

(when (boundp 'aquamacs-version)
  (unload-feature 'aquamacs-tabbar t)
  (unload-feature 'tabbar t))

(global-hl-line-mode 1)
(custom-set-faces
 '(highlight ((t (:background "#121" :foreground nil :slant normal :weight normal)))))
(custom-set-faces
 '(highline-face ((t (:background "#121" :foreground nil :slant normal :weight normal)))))
(custom-set-faces
 '(hl-line ((t (:background "#121" :foreground nil :slant normal :weight normal)))))




