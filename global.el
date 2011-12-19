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

;;-- Custom variable consistency tests --;;

;; Function that verifies a given list of variables is defined
(defun variable-list-defined (var-list)
  "Returns a possibly empty subset of the list representing the
undefined variables in the list"
  (unless (eq var-list ())
    (let ((var (car var-list))
	  (rest (cdr var-list)))
      (if (not (boundp var))
	  (cons var (variable-list-defined rest))
	(variable-list-defined rest)))))
	

;; List of block comment variables
(setq block-comment-caps '(block-comment-start block-comment-end))

;; A function that reports all undefined block comment variables. Used
;; for after-change-major-mode-hook
(defun verify-block-comment-vars ()
  "Verifies that block comment-related variables are bound."
  ;; These variables only need be set for modes with multi-line
  ;; comments
  (unless (eq comment-multi-line nil)
    (let ((undef-block-comment-vars 
	   (variables-list-defined block-comment-caps)))
      (when (not (eq undef-block-comment-vars ()))
	(message "Block comment variable(s) %s undefined"
		 undef-block-comment-vars)))))

;; Set the hook to ensure custom block comment variables are set
;; properly
(add-hook 'after-change-major-mode-hook 'verify-block-comment-vars)

;; Date format string
(set 'custom-date-format "%e %b %Y")