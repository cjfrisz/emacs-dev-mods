;; Backport the user-emacs-directory variable because sometimes I use Emacs 22
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/" ""))

;; Add the user directory to the load-path variable
(add-to-list 'load-path user-emacs-directory)

;;-- Global mode imports
(require 'ess-site)
(require 'haskell-mode)

;;-- User-local mode imports

;; Rust mode import
(add-to-list 'load-path 
	     (concat user-emacs-directory
		     (convert-standard-filename "rust-mode/")))
(require 'rust-mode)

;; Add "lisp" for custom-defined elisp functions
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "lisp/")))

;; Load custom functions
(load "lisp-load.el")

;;-- Global and miscellaneous settings --;;

;; Shortcut for goto-line
(global-set-key "\M-g" 'goto-line)

;; I find narrow-to-region useful
(put 'narrow-to-region 'disabled nil)

;; For block comments, I like to have a repeated character at the
;; beginning, end, and often throughout blocks. By default, I use the
;; dash ('-') character. I prefer different ones for certain
;; languages, and so there are hooks for those major modes.
(set 'block-comment-decorator "-")

;; Enable column number mode
(column-number-mode t)

;; And...let's show the time
(display-time)

;; Date format string
(set 'custom-date-format "%e %b %Y")

;; Set fill-column value explicitly
(set 'fill-column 70)

;; Turn on auto-fill-mode for text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Move auto-saves and backup files to the auto-save-list directory
;; Modified from the EmacsWiki
(let ((auto-save-directory 
       (concat user-emacs-directory
 	       (convert-standard-filename "auto-save-list/"))))
  (setq backup-directory-alist
 	`((".*" . ,auto-save-directory)))
  (setq auto-save-file-name-transforms
 	`((".*" ,auto-save-directory t))))

;;--------------------------------;;
;;-- Language-specific settings --;;
;;--------------------------------;;

;;-- C --;;

;; Set the block comment beginning-of-line string
(custom-set-variables '(c-block-comment-prefix "* "))

;;-- C mode common hooks

;; I prefer the '*' character for the block comment decorators in
;; languages with C-like, multi-line block comments.
(add-hook 'before-change-hook 
	  '(lambda () 
	     (let ((cur-decorator block-comment-decorator))
	       (if (eq major-mode 'c-mode)
		   (set 'block-comment-decorator "*")
		 (set 'block-comment-decorator cur-decorator)))))


;;-- Scheme --;;

;; Use petite as the default Scheme program
(custom-set-variables '(scheme-program-name "petite"))

;; Teach Emacs how to properly indent
;; certain Scheme special forms
;; (such as 'pmatch')
(put 'cond 'scheme-indent-function 0)
(put 'for-each 'scheme-indent-function 0)
(put 'pmatch 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'union-case 'scheme-indent-function 2)
(put 'cases 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 2)
(put 'syntax-case 'scheme-indent-function 2)
(put 'test 'scheme-indent-function 1)
(put 'test-check 'scheme-indent-function 1)
(put 'test-divergence 'scheme-indent-function 1)
(put 'make-engine 'scheme-indent-function 0)
(put 'with-mutex 'scheme-indent-function 1)
(put 'trace-lambda 'scheme-indent-function 1)
(put 'timed-lambda 'scheme-indent-function 1)
(put 'tlambda 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'module 'scheme-indent-function 2)
