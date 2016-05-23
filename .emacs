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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "red"))))
 '(flymake-warnline ((((class color)) (:background "yellow"))))
 '(whitespace-line ((((class color)) (:background "black"))))
 '(whitespace-tab ((((class color)) (:foreground "#505050")))))

; for c mode, tab in 8 spaces
(setq-default 
 c-default-style "linux"
 c-basic-offset 8)

; don't indent tabs
(setq-default indent-tabs-mode nil)

; disable the C-x u/l locks
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; route backups to a certain directory, copy the backups instead of mv
(setq backup-directory-alist `(("." . "~/.emacsbackups")))
(setq backup-by-copying t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/todo.org")))
 ; prevent mumamo from annoying me too much
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

; open .jsx with javascript mode
(setq auto-mode-alist (cons '("\\.jsx$" . javascript-mode) auto-mode-alist))

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

; get pyflake into the mix
(toss
 (require 'flymake)
 (when (load "flymake" t)
   (defun flymake-pylint-init ()
     (let* ((temp-file (flymake-init-create-temp-buffer-copy
                        'flymake-create-temp-inplace))
            (local-file (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
       (list "epylint" (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
                '("\\.py\\'" flymake-pylint-init)))
 (custom-set-faces
  '(flymake-errline ((((class color)) (:background "red"))))
  '(flymake-warnline ((((class color)) (:background "yellow")))))
 ; from https://github.com/illusori/emacs-flymake-cursor
 (add-to-list 'load-path "~/.emacs.d/emacs-flymake-cursor")
 (require 'flymake-cursor)
 (add-hook 'find-file-hook 'flymake-find-file-hook)
)

(toss
 (add-to-list 'load-path "~/.emacs.d/")
 (require 'clojure-mode))

;; go mode
; installed with golang-mode (ubuntu packages)
;; (toss
;;  ; usual ubuntu golang install path
;;  (setq load-path (cons "/usr/lib/go/misc/emacs" load-path))
;;  (require 'go-mode-load)
;;  (add-hook 'before-save-hook #'gofmt-before-save))

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

;; lilypond mode
; part of ubuntu's lilypond package
(toss
 (require 'lilypond-mode))

;;##############################################################################
; org-mode

(require 'org)
(require 'outline)

(define-key global-map "\C-ca" 'org-agenda)
; extend today until 5am
(setq org-extend-today-until 5)
; Have start/end of line take org-mode decorations into account
(setq org-special-ctrl-a t)
(setq org-special-ctrl-e t)
; Sort agenda tasks
(setq org-agenda-sorting-strategy '(todo-state-up timestamp-down))

; Pomodoros, pulled from http://orgmode.org/worg/org-gtd-etc.html
(add-to-list 'org-modules 'org-timer)
(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook (lambda ()
      (if (not org-timer-current-timer)
      (org-timer-set-timer '(16)))))

; Switch to in progress if we're logging time on a todo.
; We don't use org-clock-in-switch-to-state because it's too naive.
(add-hook 'org-clock-in-hook
          (lambda ()
            ; Currently on the clock element, move back to headline.
            (outline-back-to-heading)
            (let* ((current-headline (org-element-at-point))
                   (current-status
                    (org-element-property :todo-keyword current-headline)))
              (if (not (string= "INPROGRESS" current-status))
                  (org-todo "INPROGRESS")
                )
              )
            )
          )

; Agenda commands.
(setq org-agenda-custom-commands
      '(; View active top level projects
        ("h" tags-todo "+LEVEL=1+TODO={TODO\\|INPROGRESS}")
        ; View all in progress tasks
        ("d" tags-todo "+TODO={TODO\\|INPROGRESS}")
        ; View sleeping tasks
        ("1" tags-todo "+TODO=\"SL1W\"")
        ("2" tags-todo "+TODO=\"SL1M\"")
        ("3" tags-todo "+TODO=\"SL3M\"")
        ("4" tags-todo "+TODO=\"SL1Y\"")
        ("5" tags-todo "+TODO=\"SL5Y\"")
        ("'" tags-todo "+LEVEL=1+TODO=\"SL1W\"")
        ("," tags-todo "+LEVEL=1+TODO=\"SL1M\"")
        ("." tags-todo "+LEVEL=1+TODO=\"SL3M\"")
        ("p" tags-todo "+LEVEL=1+TODO=\"SL1Y\"")
        ("y" tags-todo "+LEVEL=1+TODO=\"SL5Y\"")
        ; View weekly timesheet
        ("l" "Weekly Time Log"
         ((agenda ""))
         ((org-agenda-overriding-header "Weekly time log")
          (org-agenda-span 'week)
          (org-agenda-log-mode)
          (org-agenda-show-log 'clockcheck) ; t
          )
         )
        ; View daily clocktable
        ("r" "Weekly timesheet"
         ((agenda ""))
         ((org-agenda-overriding-header "Weekly review")
          (org-agenda-span 'week)
          (org-agenda-start-with-clockreport-mode t)
          )
         )
        )
      )

; Track effort changes
(add-hook 'org-property-changed-functions
          (lambda (property value)
            (if (string= property "Effort")
                (save-excursion
                  (org-back-to-heading)
                  (let* ((current-headline (org-element-at-point))
                         (headline-level
                          (org-element-property :level current-headline)))
                    ; Make sure that we have space to edit in.
                    (org-reveal)
                    (end-of-line nil)
                    (insert "\n")
                    ; Indent the log.
                    (insert (make-string (+ 1 headline-level) ? ))
                    (insert (org-list-bullet-string "-") "Effort changed to ")
                    (insert value)
                    ; Indent the timestamp.
                    (indent-to-column (+ 40 headline-level))
                    (org-insert-time-stamp (current-time) t t)
                    )
                  )
              )
            )
          )

; Switch states when refiling.
;; (add-hook 'org-after-refile-insert-hook
;;           (lambda ()
;;             (let ((current-headline (org-element-at-point)))
;;               (org-up-heading-safe)
;;               (let ((parent-level
;;                      (org-element-property :level (org-element-at-point)))
;;                     (parent-title
;;                      (org-element-property :title (org-element-at-point))))
;;                 (if (eq parent-level 1)
;;                     (progn
;;                       (goto-char (org-element-property :begin current-headline))
;;                       (cond
;;                        ((string= parent-title "Input")
;;                         (org-todo "?"))
;;                        ((string= parent-title "Active")
;;                         (org-todo "⚡"))
;;                        ((string= parent-title "Next (1w)")
;;                         (org-todo "𝄿"))
;;                        ((string= parent-title "Upcoming (1m)")
;;                         (org-todo "𝄾"))
;;                        ((string= parent-title "Planned (3m)")
;;                         (org-todo "𝄽"))
;;                        ((string= parent-title "Someday (1y)")
;;                         (org-todo "𝄻"))
;;                        ((string= parent-title "Maybe")
;;                         (org-todo "𝄐"))
;;                        ((string= parent-title "Done")
;;                         (org-todo "✔"))
;;                        ((string= parent-title "Archived")
;;                         (org-todo "♻"))
;;                        )
;;                       )
;;                   )
;;                 )
;;               )
;;             )
;;           )

; Sort non-top level todos on state changes.
;; (add-hook 'org-after-todo-state-change-hook
;;           (lambda ()
;;             (save-excursion ; probably doesn't work due to structure changes
;;               (org-up-heading-safe)
;;               (org-sort-entries t ?o))
;;             )
;;           )
; See http://emacs.stackexchange.com/a/10276
