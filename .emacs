;; WHAT CL COMPAT???
(require 'cl)

; don't care about the result
(defmacro toss (&rest body)
  `(condition-case ex
       (progn ,@body)
     ('error)))

;;######################################################################
;; defaults

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ; Flymake faces
 '(flymake-errline ((((class color)) (:background "red"))))
 '(flymake-warnline ((((class color)) (:background "yellow"))))

 ; whitespace highlighting
 '(whitespace-line ((((class color)) (:background "black"))))
 '(whitespace-tab ((((class color)) (:foreground "#505050"))))
)

; try to set the default fonts
(set-face-attribute 'default nil :height 110)

; fancy whitespace highlighting
(require 'whitespace)
(setq whitespace-style '(face empty tabs tab-mark lines-tail))
(global-whitespace-mode t)

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
; Prevent invisible edits in folded text
(setq org-catch-invisible-edits 'smart)
; Play a sound when clocking in/timing out
(setq org-clock-sound "/home/thenoviceoof/.local/lib/bell.wav")
; Include current task in clock reports
(setq org-clock-report-include-clocking-task t)

;; Sorting agenda tasks
; We define our own iats extractor instead of using tsia-down, which
; requires the timestamp to be used directly in the headline text.
(setq thenoviceoof/iats-regex-start
      (concat "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [a-ZA-Z]\\{3\\} "
              "[0-9]\\{2\\}:[0-9]\\{2\\}\\]"))
(setq thenoviceoof/min-time-string "1970-01-01 00:00")
(defun thenoviceoof/task-extract-max-iats (task)
  ; Extract the buffer/position of the task from the task string
  (let ((task-marker (get-text-property 1 'org-marker task)))
    (if task-marker
      (with-current-buffer (marker-buffer task-marker)
        (goto-char (marker-position task-marker))
        ; Get the bounds of the headline, including children
        (let* ((task-elem (org-element-at-point))
               (task-start (org-element-property :begin task-elem)))
          (org-forward-heading-same-level 1)
          (let* ((next-elem (org-element-at-point))
                 (next-begin (or (org-element-property :begin next-elem)
                                 (point-max)))
                 (max-time-string thenoviceoof/min-time-string))
            (goto-char task-start)
            ; Look at each timestamp, get the max
            (while (re-search-forward thenoviceoof/iats-regex-start next-begin t)
              (let* ((ts-end (point))
                     ; Assume the timestamp always takes the same length
                     (ts-start (- ts-end 22))
                     (ts-str (buffer-substring (+ ts-start 1) ts-end))
                     ; Tear out the day of the week
                     (ts-front (substring ts-str 0 10))
                     (ts-back (substring ts-str -7 -1))
                     (ts-no-dow (concat ts-front ts-back)))
                (if (string< max-time-string ts-no-dow)
                    (setq max-time-string ts-no-dow))
                )
              )
            max-time-string
            )
          )
        )
      ; if task-marker is nil
      thenoviceoof/min-time-string
      )
    )
  )
(defun thenoviceoof/task-iats-cmp (taska taskb)
  (let ((taska-max-iats (thenoviceoof/task-extract-max-iats taska))
        (taskb-max-iats (thenoviceoof/task-extract-max-iats taskb)))
    (cond
     ((string< taska-max-iats taskb-max-iats)
      -1)
     ((string< taskb-max-iats taska-max-iats)
      1)
     (t 0)
     )
    )
  )
(setq org-agenda-cmp-user-defined 'thenoviceoof/task-iats-cmp)

; Always highlight the current agenda line
; (from http://doc.norang.ca/org-mode.html#HighlightCurrentAgendaLine)
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

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
        ("h" tags-todo "+LEVEL=1+TODO={TODO\\|INPROGRESS}"
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ; View all in progress tasks
        ("d" tags-todo "+TODO={TODO\\|INPROGRESS}"
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ; View sleeping tasks
        ("1" tags-todo "+TODO=\"SL1W\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("2" tags-todo "+TODO=\"SL1M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("3" tags-todo "+TODO=\"SL3M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("4" tags-todo "+TODO=\"SL1Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("5" tags-todo "+TODO=\"SL5Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("'" tags-todo "+LEVEL=1+TODO=\"SL1W\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("," tags-todo "+LEVEL=1+TODO=\"SL1M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("." tags-todo "+LEVEL=1+TODO=\"SL3M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("p" tags-todo "+LEVEL=1+TODO=\"SL1Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
        ("y" tags-todo "+LEVEL=1+TODO=\"SL5Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))))
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

; Check if we have open clocks before closing.
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (let ((close-editor t))
              (dolist (buffer (buffer-list))
                (let ((major-mode (with-current-buffer buffer major-mode)))
                  (if (string= "org-mode" major-mode)
                      (with-current-buffer buffer
                        (if (org-clock-is-active)
                            ; active clock and yes, so return nil
                            (if (y-or-n-p "Running clock, close?")
                                (setq close-editor t)
                              (setq close-editor nil)
                              )
                          )
                        )
                    )
                  )
                )
              ; return the value
              close-editor
              )
            )
          )
