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
(setq whitespace-style '(face empty tabs lines-tail))
(global-whitespace-mode t)

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

; trying out IDO, per Moses's suggestion
(require 'ido)


;;######################################################################
;; language specific extensions

; open .json with javascript mode
(setq auto-mode-alist (cons '("\\.json$" . javascript-mode) auto-mode-alist))

; scala mode
(toss
 (add-to-list 'load-path "~/.emacs.d/scala-mode/")
 (require 'scala-mode)
 (require 'scala-mode-auto))

; coffee mode
(toss
 (add-to-list 'load-path "~/.emacs.d/coffee-mode")
 (require 'coffee-mode)
 (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
 (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode)))

; caml-font
(toss
 (require 'caml-font)
 (setq save-abbrevs nil))

; NOTE: do not install python-mode through apt, do install pymacs
(toss
 ; ropemacs
 (require 'pymacs)
 (pymacs-load "ropemacs" "rope-")
 (setq ropemacs-enable-autoimport t))

(toss
 (add-to-list 'load-path "~/.emacs.d/")
 (require 'clojure-mode))

;; go mode
(toss
 ; usual ubuntu golang install path
 (setq load-path (cons "/usr/lib/go/misc/emacs" load-path))
 (require 'go-mode-load))
