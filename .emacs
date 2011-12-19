;; Backport the user-emacs-directory variable because sometimes I use Emacs 22
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/" ""))

;; Add the user directory to the load-path variable
(add-to-list 'load-path user-emacs-directory)

;; Mode imports
(require 'ess-site)
(require 'haskell-mode)

;;-------------------;;
;; Custom load files ;;
;;-------------------;;

;; Global and miscellaneous settings
(let ((misc-decls (concat user-emacs-directory
			 (convert-standard-filename "global.el"))))
  (load-file misc-decls))

;;-- Language-specific settings --;;
(let ((lang-path (concat user-emacs-directory 
			 (convert-standard-filename "lang/"))))
  (let ((c-decls (concat lang-path 
			 (convert-standard-filename "c.el")))
	(scheme-decls (concat lang-path
			      (convert-standard-filename "scheme.el"))))
    (load-file c-decls)
    (load-file scheme-decls)))

;;-- Custom functions --;;
(let ((lisp-path (concat user-emacs-directory
			 (convert-standard-filename "lisp/"))))
  (let ((load-lisp (concat lisp-path
			   (convert-standard-filename "load.el"))))
    (load-file load-lisp)));; Test comment
