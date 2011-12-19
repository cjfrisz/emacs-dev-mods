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

;;-- Additional load-path directories

;; emacs-user-directory for top-level of customized files
(add-to-list 'load-path user-emacs-directory)
;; lang directory for language-specific customizations
(add-to-list 'load-path 
	     (concat user-emacs-directory
		     (convert-standard-filename "lang/")))
;; lisp for custom-defined elisp functions
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "lisp/")))

;; Load global and miscellaneous settings (in top-level
;; user-emacs-directory)
(load "global.el")

;;-- Language-specific settings --;;
(load "lang-load.el")

;;-- Custom functions --;;
(load "lisp-load.el")
