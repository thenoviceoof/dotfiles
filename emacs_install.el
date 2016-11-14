;; TODO: consider adding org-mode

; Set up the package manager on Emacs 24
(require 'package)
(add-to-list 'package-archives
             ; TODO: change the http to https once Emacs supports it.
             '("MELPA Stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

; Get the latest package list.
(package-refresh-contents)

; ------------------------------------------------------------------------------
; Install stuff

(package-install 'haskell-mode)

; Mode for R
(package-install 'ess)

(package-install 'coffee-mode)

(package-install 'clojure-mode)

(package-install 'rust-mode)

(package-install 'go-mode)

(package-install 'less-css-mode)

(package-install 'scala-mode)

