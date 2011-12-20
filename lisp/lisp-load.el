;;----------------------------------------------------------------------
;; File lisp-load.el
;; Written by Chris Frisz
;; 
;; Created 17 Dec 2011
;; Last modified 20 Dec 2011
;; 
;; Loads the extra elisp files I've defined in the ~/.emacs.d/lisp
;; directory
;; ----------------------------------------------------------------------

;;-- Load files --;;
;; NOTE: This requires that the "lisp" directory has been added to the
;; laod-path variable

;; Use helpers.el for the "load-file-list" function
(load "helpers.el")

;; The list of custom elisp files
(setq lisp-files '("file-doc.el"
		   "helpers.el" ; Redundant, I know
		   "update-modify.el"))

;; Perform the actual load
(load-file-list lisp-files)

;;-- Additional configuration --;;

;; Set the before-save hook to update the modify date
(add-hook 'before-save-hook 'update-modify)
