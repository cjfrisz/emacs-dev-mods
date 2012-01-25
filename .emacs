;;----------------------------------------------------------------------
;; File .emacs
;; Written by Chris Frisz
;; 
;; Created 17 Dec 2011
;; Last modified 25 Jan 2012
;; 
;; Initialization file for Emacs.
;;----------------------------------------------------------------------

;; Backport the user-emacs-directory variable because sometimes I use Emacs 22
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/" ""))

;; Add the user directory to the load-path variable
(add-to-list 'load-path user-emacs-directory)

;;-- Global mode imports
;; (require 'ess-site)
;; (require 'haskell-mode)

;;-- User-local mode imports

;; Rust mode import
;; (add-to-list 'load-path 
;; 	     (concat user-emacs-directory
;; 		     (convert-standard-filename "rust-mode/")))
;; (require 'rust-mode)

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
(set 'cjfrisz-date-format "%e %b %Y")

;; Set fill-column value explicitly
(set 'fill-column 70)

;; Default to text mode if nothing else overrides it
(set-default 'major-mode 'text-mode)

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

;; Highlight parentheses in all modes
;; Taken from the EmacsWiki
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; Useful definitions for replacing character sequences with unicode
;; characters, i.e. replace the word "lambda" with the Greek letter.
;; Adapted from "PrettySymbolsForLanguages" page on the Emacs wiki:
;; http://www.emacswiki.org/emacs/PrettySymbolsForLanguages
  (defun unicode-symbol (name)
    "Translate a symbolic name for a Unicode character -- e.g.,
     LEFT-ARROW or GREATER-THAN into an actual Unicode character
     code. "
    (decode-char 'ucs (case name
                        ;; arrows
                        ('left-arrow 8592)
                        ('up-arrow 8593)
                        ('right-arrow 8594)
                        ('down-arrow 8595)
                        ;; boxes
                        ('double-vertical-bar #X2551)
                        ;; relational operators
                        ('equal #X003d)
                        ('not-equal #X2260)
                        ('identical #X2261)
                        ('not-identical #X2262)
                        ('less-than #X003c)
                        ('greater-than #X003e)
                        ('less-than-or-equal-to #X2264)
                        ('greater-than-or-equal-to #X2265)
                        ;; logical operators
                        ('logical-and #X2227)
                        ('logical-or #X2228)
                        ('logical-neg #X00AC)
                        ;; misc
                        ('nil #X2205)
                        ('horizontal-ellipsis #X2026)
                        ('double-exclamation #X203C)
                        ('prime #X2032)
                        ('double-prime #X2033)
                        ('for-all #X2200)
                        ('there-exists #X2203)
                        ('element-of #X2208)
                        ;; mathematical operators
                        ('square-root #X221A)
                        ('squared #X00B2)
                        ('cubed #X00B3)
                        ;; letters
                        ('lambda #X03BB)
                        ('alpha #X03B1)
                        ('beta #X03B2)
                        ('gamma #X03B3)
                        ('delta #X03B4))))

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN
   with the Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern (0 (progn (compose-region (match-beginning 1)
                                             (match-end 1)
                                             ,(unicode-symbol
                                               symbol))
                             nil))))))
  
(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))


;;--------------------------------;;
;;-- Language-specific settings --;;
;;--------------------------------;;

;;-- C/C++ --;;

;; Set the block comment beginning-of-line string
(custom-set-variables '(c-block-comment-prefix "* "))

;; I prefer auto-fill-mode for C/C++
(add-hook 'c-mode-common-hook 'auto-fill-mode)


;;-- Lisp (including Emacs Lisp) --;;

;; Indentation rules
;; I really don't know why these aren't defined correctly
(put 'case 'lisp-indent-function 1)
(put 'if 'lisp-indent-function 3)

;; Auto-fill-mode for Emacs Lisp is nice, too
(add-hook 'emacs-lisp-mode-hook 'auto-fill-mode)

;; Balanced paren mode for Emacs Lisp
(add-hook 'emacs-list-mode-hook 'balanced-on)


;;-- Scheme --;;

;; Use petite as the default Scheme program
;; (custom-set-variables '(scheme-program-name "petite"))

;; IU Scheme setup
(autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)
 
;; Insane Scheme setup (balanced paren mode)
(autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t)
(autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t)
(add-hook 'scheme-mode-hook 'balanced-on)

;; Balanced paren mode for Inferior Scheme process
(add-hook 'inferior-scheme-mode-hook 'balanced-on)

;; Use auto-fill-mode for Scheme
(add-hook 'scheme-mode-hook 'auto-fill-mode)

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
(put 'values 'scheme-indent-function 0)

;; Replace the word lambda with the unicode character for the
;; lower-case Greek letter
(defun scheme-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "\\(lambda\\)" 'lambda))))

(add-hook 'scheme-mode-hook 'scheme-unicode)

;; Let's also do unicode lambdas in inferior Scheme mode
(add-hook 'inferior-scheme-mode-hook 'scheme-unicode)