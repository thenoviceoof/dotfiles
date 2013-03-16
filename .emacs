;; WHAT CL COMPAT???
(require 'cl)

; don't care about the result
(defmacro toss (&rest body)
  `(condition-case ex
       (progn ,@body)
     ('error)))

;;######################################################################
;; defaults

; try to set the default fonts
(set-face-attribute 'default nil :height 110)

; fancy whitespace highlighting
(require 'whitespace)
(setq whitespace-style '(face empty tabs tab-mark lines-tail))
(global-whitespace-mode t)
(custom-set-faces
 '(whitespace-tab (( ((class color))
                     (:foreground "#505050"))))
 '(whitespace-line (( ((class color))
                      (:background "black")))) )

; for c mode, tab in 8 spaces
(setq-default 
 c-default-style "linux"
 c-basic-offset 8)

; don't indent tabs
(setq-default indent-tabs-mode nil)


;;######################################################################
;; more fancy language-agnostic things

; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

; try a better expansion lib
(toss
 (global-set-key "\M- " 'hippie-expand))

; trying out predictive mode
(toss
 (add-to-list 'load-path "~/.emacs.d/predictive")
 (require 'predictive)
 (autoload 'predictive-mode "predictive" "predictive" t)
 (set-default 'predictive-auto-add-to-dict t)
 (setq ;predictive-main-dict 'rpg-dictionary ; don't have a dict
  predictive-auto-learn t
  predictive-add-to-dict-ask nil
  predictive-use-auto-learn-cache nil
  predictive-which-dict t))

; get rid of the annoying revert on git branch switches
(global-auto-revert-mode t)

;; trying out IDO, per Moses's suggestion
; http://emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)

; prevent mumamo from annoying me too much
(custom-set-variables
 '(warning-minimum-level :error))


;;######################################################################
;; language specific extensions

;; mumamo, for html+js
; http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html+js
(toss
  (load "~/.emacs.d/nxhtml/autostart.el")
  (setq mumamo-background-colors nil))

; open .zsh with shell-script mode
(setq auto-mode-alist (cons '("\\.zsh$" . shell-script-mode) auto-mode-alist))

; open .json with javascript mode
(setq auto-mode-alist (cons '("\\.json$" . javascript-mode) auto-mode-alist))

; scala mode
(toss
 (add-to-list 'load-path "~/.emacs.d/scala-mode/")
 (require 'scala-mode)
 (require 'scala-mode-auto))

;; coffee mode
; https://github.com/defunkt/coffee-mode
(toss
 (add-to-list 'load-path "~/.emacs.d/coffee-mode")
 (require 'coffee-mode)
 (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
 (add-to-list 'auto-mode-alist '("\\.iced$" . coffee-mode))
 (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode)))

; caml-font
(toss
 (require 'caml-font)
 (setq save-abbrevs nil))

; NOTE: do not install python-mode through apt, do apt install pymacs
(toss
 ; ropemacs
 (require 'pymacs)
 (pymacs-load "ropemacs" "rope-")
 (setq ropemacs-enable-autoimport t))

(toss
 (add-to-list 'load-path "~/.emacs.d/")
 (require 'clojure-mode))

;; go mode
; installed with golang (ubuntu packages)
(toss
 ; usual ubuntu golang install path
 (setq load-path (cons "/usr/lib/go/misc/emacs" load-path))
 (require 'go-mode-load))

;; jade mode
; https://github.com/brianc/jade-mode
(toss
 (add-to-list 'load-path "~/.emacs.d/jade-mode")
 (require 'jade-mode)
 (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode)))

;; less mode
; https://github.com/purcell/less-css-mode/
(toss
  (require 'less-css-mode)
  (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode)))
