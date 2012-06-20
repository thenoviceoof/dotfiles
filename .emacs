; try to set the default fonts
(set-face-attribute 'default nil :height 110)

; fancy whitespace highlighting
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail))
(global-whitespace-mode t)

; for c mode, tab in 8 spaces
(setq-default 
 c-default-style "linux"
 c-basic-offset 8)

(require 'caml-font)
(setq save-abbrevs nil)

; try a better expansion lib
(global-set-key "\M- " 'hippie-expand)

; don't indent tabs
(setq-default indent-tabs-mode nil)

; trying out predictive mode
(add-to-list 'load-path "~/.emacs.d/predictive")
(require 'predictive)
(autoload 'predictive-mode "predictive" "predictive" t)
(set-default 'predictive-auto-add-to-dict t)
(setq ;predictive-main-dict 'rpg-dictionary ; don't have a dict
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

; scala mode
(add-to-list 'load-path "/home/thenoviceoof/.emacs.d/scala-mode/")
(require 'scala-mode)
(require 'scala-mode-auto)

; open .json with javascript mode
(setq auto-mode-alist (cons '("\\.json$" . javascript-mode) auto-mode-alist))