;; WHAT CL COMPAT???
(require 'cl)

; don't care about the result
(defmacro toss (&rest body)
  `(condition-case ex
       (progn ,@body)
     ('error)))

; Set up the package manager on Emacs 24
(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;;######################################################################
;; defaults

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ; The default blue color is a bit too dark. Use a tweaked Tango color
 '(font-lock-function-name-face ((((class color)) (:foreground "#2455F4"))))
 '(org-level-1-face ((((class color)) (:foreground "#2455F4"))))
 '(org-agenda-structure ((((class color)) (:foreground "#2455F4"))))

 ; Flymake faces
 '(flymake-errline ((((class color)) (:background "red"))))
 '(flymake-warnline ((((class color)) (:background "yellow"))))

 ; whitespace highlighting
 '(whitespace-line ((((class color)) (:background "black"))))
 '(whitespace-tab ((((class color)) (:foreground "#505050"))))

 ; hl-mode
 '(hl-line ((((class color)) (:background "slate gray"))))
)

; try to set the default fonts
(set-face-attribute 'default nil :height 110)

; fancy whitespace highlighting
(require 'whitespace)
(setq whitespace-style '(face empty tabs tab-mark lines-tail))
(global-whitespace-mode t)
(setq whitespace-global-modes '(not org-mode))

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
 '(org-agenda-files (quote ("~/org/todo.org")))
 ; prevent mumamo from annoying me too much
 '(warning-minimum-level :error))


;;######################################################################
;; language specific extensions

; open .zsh with shell-script mode
(setq auto-mode-alist (cons '("\\.zsh$" . shell-script-mode) auto-mode-alist))

; open .json/.jsx with javascript mode
(setq auto-mode-alist (cons '("\\.json$" . javascript-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jsx$" . javascript-mode) auto-mode-alist))

;; coffee mode
(toss
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

;; go mode
(toss
 (add-hook 'before-save-hook #'gofmt-before-save))

;; jade mode
; https://github.com/brianc/jade-mode
(toss
 (add-to-list 'load-path "~/.emacs.d/jade-mode")
 (require 'jade-mode)
 (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode)))

;; lilypond mode
; part of ubuntu's lilypond package
(toss
 (require 'lilypond-mode))

;; haskell mode
; this just turns on indentation
(toss
 (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

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
; Todo keywords
(setq org-todo-keywords
      '((sequence "?(i!)" "INPROGRESS(g!)" "TODO(t!)" "EXT(e!)"
                  "SL1W(n!)" "SL1M(u!)" "SL3M(p!)" "SL1Y(s!)" "SL5Y(m!)"
                  "|" "DONE(d!)" "ARCHIVED(a!)")))
; Use non-default colors for sleeping states
(setq org-todo-keyword-faces
      '(("SL1W" . "magenta")
        ("SL1M" . "magenta")
        ("SL3M" . "magenta")
        ("SL1Y" . "magenta")
        ("SL5Y" . "magenta"))
      )
; Use the full width of a standard buffer on 1920p (bujano).
(setq org-tags-column -105)
(setq org-agenda-tags-column 100)
; Use F9 for C-c a
(global-set-key (kbd "<f9>") 'org-agenda)
; Remove "Clocking gap" messages
(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "24:00"
              :min-duration 0
              :max-gap "168:00"  ; A week; longer gaps get marked
              :gap-ok-around ("4:00"))))  ; Middle of the night, to
                                          ; suppress warnings.
; Set up MobileOrg
(setq org-mobile-directory "/orgmode@raspberrypi.local:/data/current/org")
(setq org-mobile-inbox-for-pull "/home/thenoviceoof/org/input.org")

; Edit agenda text before display
(defun thenoviceoof/substring-or-pad (string start end)
  "Depending on the cut to make, either pad the end of the
string, or substring it."
  (if (< (length string) end)
      (let* ((padding (- end (length string)))
             (string (concat string (make-string padding ? ))))
        (substring string start end)
        )
    (substring string start end)
    )
  )
(defun thenoviceoof/org-agenda-line-prefix-parent-title (prefix-length)
  "Return the truncated parent title (if there is one), or an empty string."
  (save-excursion
    (if (org-up-heading-safe)
        (let* ((parent-elem (org-element-at-point))
               (parent-title (org-element-property :title parent-elem))
               (str (thenoviceoof/substring-or-pad parent-title
                                                   0 prefix-length)))
          str
          )
      (make-string prefix-length ? )
      )
    )
  )
(defun thenoviceoof/org-agenda-line-prefix-childrenp ()
  "Return whether this task has children, or nil"
  (save-excursion
    (let ((this-point (point)))
      (org-goto-first-child)
      (if (not (equalp (point) this-point))
          "⬇"
        nil)
      )
    )
  )
(defun thenoviceoof/org-effort-sum ()
  "Sum all the efforts from the available headings"
  (save-excursion
    (let ((previous-point -1)
          (total-minutes 0))
      (while (not (= previous-point (point)))
        (let* ((effort (or (org-entry-get (point) "Effort") "0:00")))
          (setq total-minutes (+ total-minutes
                                 (org-hh:mm-string-to-minutes effort)))
          )
        (setq previous-point (point))
        (outline-next-visible-heading 1)
        )
      total-minutes
      )
    )
  )
(defun thenoviceoof/org-agenda-line-prefix-work-done ()
  "Add a unicode meter indicating how much work was done."
  (save-restriction
    (org-narrow-to-subtree)
    (let ((clocked-time (org-clock-sum))
          (effort-time (thenoviceoof/org-effort-sum)))
      ; Calculate fractions
      (if (and effort-time clocked-time
               (< 0 effort-time) (< 0 clocked-time))
          (let* ((fraction (/ (float clocked-time) effort-time)))
            (cond ((< fraction 0.001) " ")
                  ((< fraction 0.125) "▁")
                  ((< fraction 0.250) "▂")
                  ((< fraction 0.375) "▃")
                  ((< fraction 0.500) "▄")
                  ((< fraction 0.625) "▅")
                  ((< fraction 0.750) "▆")
                  ((< fraction 0.875) "▇")
                  ; Overdue task
                  ((> fraction 1.500) "▙")
                  (t                  "█")))
        nil
        )
      )
    )
  )
(defun thenoviceoof/org-agenda-prefix (&optional str-length)
  (if (not str-length)
      (setq str-length 15))
  (let* ((str-suffixes (list (thenoviceoof/org-agenda-line-prefix-childrenp)
                             (thenoviceoof/org-agenda-line-prefix-work-done)
                             "|"))
         (str-suffix (reduce #'concat (remove-if #'null str-suffixes)))
         (str-prefix-length (- str-length (length str-suffix)))
         (str-prefix
          (thenoviceoof/org-agenda-line-prefix-parent-title str-prefix-length)))
    (concat str-prefix str-suffix)
    )
  )

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
            ; NOTE The first condition prevents "Invalid search bound
            ; (wrong side of point)"
            (while (and (< (point) next-begin)
                        (re-search-forward thenoviceoof/iats-regex-start next-begin t))
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

; Agenda pre-processing
(defun thenoviceoof/org-before-sorting-filter (task)
  (let* ((max-iats (thenoviceoof/task-extract-max-iats task)))
    ; Set the iats for later use
    (put-text-property 1 (length task)
                       'thenoviceoof/org-child-max-iats
                       max-iats
                       task)
    task
    )
  )
(setq org-agenda-before-sorting-filter-function
      'thenoviceoof/org-before-sorting-filter)

; Sorting agenda tasks
(defun thenoviceoof/task-iats-cmp (taska taskb)
  (let ((taska-max-iats
         (get-text-property 1 'thenoviceoof/org-child-max-iats taska))
        (taskb-max-iats
         (get-text-property 1 'thenoviceoof/org-child-max-iats taskb)))
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
(add-hook 'org-clock-in-hook
          (lambda ()
            (if (not org-timer-countdown-timer)
                ; 16 is simply a magic value, for replacing the current timer.
                ; Current as of 9.0, defined within the function itself.
                (org-timer-set-timer '(16)))
            )
          )
(add-hook 'org-clock-out-hook
          (lambda ()
            (if org-timer-current-timer
                (progn
                  (org-timer-stop)
                  ; Fix a bug with the current timer continuing. Fixed
                  ; at org-mode HEAD.
                  ; TODO: remove after upgrading org-mode past 173b0cb6
                  ; (Dec 2014)
                  (if org-timer-current-timer
                      (progn
                        (cancel-timer org-timer-current-timer)
                        (setq org-timer-current-timer nil))
                    )
                  )
              )
            )
          )

; Propagate state changes to parents, based on state of children
(defun thenoviceoof/org-todo (state)
  (let ((current-state
         (org-element-property :todo-keyword (org-element-at-point))))
    (if (not (string= state current-state))
        (org-todo state))
    )
  )
(defun thenoviceoof/org-forward-heading-same-level (arg)
  (let ((previous-point (point)))
    (org-forward-heading-same-level arg)
    (not (= previous-point (point)))
    )
  )
(defun thenoviceoof/string=-prefix (stringa stringb)
  "Return true if one string is a prefix of the other"
  (or (string-prefix-p stringa stringb)
      (string-prefix-p stringb stringa))
  )
(defun thenoviceoof/org-state-change-parent ()
  (save-excursion
    (if (org-up-heading-safe)
        ; cdr of org-todo-keywords is a hack: strips out sequence/type
        ; as a bare symbol
        (let* ((todo-keywords (cdr (nth 0 org-todo-keywords)))
               (max-state-index (length todo-keywords))
               (smallest-state-index max-state-index)
               ; Special casing: switch to in progress if any tasks
               ; are already done.
               (finished-state-index
                (cl-position "|" todo-keywords :test 'string=))
               (finished-children-p nil)
               (more-children t))
          (save-excursion
            ; Iterate over children
            (org-goto-first-child)
            (while more-children
              (let* ((child-elem (org-element-at-point))
                     (child-state
                      (org-element-property :todo-keyword child-elem)))
                (if child-state
                    (let ((child-state-index
                           (cl-position child-state
                                        todo-keywords
                                        :test 'thenoviceoof/string=-prefix)))
                      ; Simply use the ordering of the todo keywords
                      (if (< child-state-index smallest-state-index)
                          (setq smallest-state-index child-state-index))
                      ; Check if there are any done states
                      (if (< finished-state-index child-state-index)
                          (setq finished-children-p t))
                      )
                  )
                ; Go to the next iteration
                (setq more-children
                      (thenoviceoof/org-forward-heading-same-level 1))
                )
              )
            )
          ; Update the parent
          (if (not (= smallest-state-index max-state-index))
              (let* ((smallest-state (nth smallest-state-index
                                          todo-keywords))
                     ; Strip out any suffix (ex TODO(t))
                     (state-suffix-maybe (string-match "(" smallest-state))
                     (without-suffix (if state-suffix-maybe
                                         (substring smallest-state
                                                    0 state-suffix-maybe)
                                       (smallest-state))))
                ; Special case handling of mixed done/todo
                (if (and finished-children-p (string= without-suffix "TODO"))
                    (thenoviceoof/org-todo "INPROGRESS")
                  (thenoviceoof/org-todo without-suffix)
                  )
                )
            )
          )
      )
    )
  )
(add-hook 'org-after-todo-state-change-hook
          'thenoviceoof/org-state-change-parent)

; Switch to in progress if we're logging time on a todo.
; We don't use org-clock-in-switch-to-state because it's too naive.
(add-hook 'org-clock-in-hook
          (lambda ()
            ; Currently on the clock element, move back to headline.
            (outline-back-to-heading)
            (thenoviceoof/org-todo "INPROGRESS")
            )
          )

; Agenda commands.
(setq org-agenda-custom-commands
      '(; View active top level projects
        ("h" "Current Projects"
         ((tags-todo "+LEVEL=1+TODO={EXT}")
          (tags-todo "+LEVEL=1+TODO={TODO\\|INPROGRESS}"))
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix 5)")))
        ; View all in progress tasks
        ("d" tags-todo "+TODO={TODO\\|INPROGRESS}"
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix)")))
        ; View sleeping tasks
        ("1" tags-todo "+TODO=\"SL1W\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix)")))
        ("2" tags-todo "+TODO=\"SL1M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix)")))
        ("3" tags-todo "+TODO=\"SL3M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix)")))
        ("4" tags-todo "+TODO=\"SL1Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix)")))
        ("5" tags-todo "+TODO=\"SL5Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix)")))
        ("'" tags-todo "+LEVEL=1+TODO=\"SL1W\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix 5)")))
        ("," tags-todo "+LEVEL=1+TODO=\"SL1M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix 5)")))
        ("." tags-todo "+LEVEL=1+TODO=\"SL3M\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix 5)")))
        ("p" tags-todo "+LEVEL=1+TODO=\"SL1Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix 5)")))
        ("y" tags-todo "+LEVEL=1+TODO=\"SL5Y\""
         ((org-agenda-sorting-strategy '(todo-state-up user-defined-up))
          (org-agenda-prefix-format "%(thenoviceoof/org-agenda-prefix 5)")))
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

; Check if we have open clocks before closing the editor.
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
