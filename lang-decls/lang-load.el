;;-- Load files --;;
;; NOTE: This requires that the "lang" directory has been added to the
;; load-path variable

;; This file uses the "load-file-list" function from helpers.el
;; Note that this requires its directory to be part of the load-path
;; variable
(load "helpers.el")

;; The list of defined language files
(setq lang-files '("c-decls.el"
		   "lisp-decls.el"
		   "scheme-decls.el"))


;; Perform the actual load
(load-file-list lang-files)
