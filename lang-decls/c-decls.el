;; Set the block comment beginning-of-line string
(custom-set-variables '(c-block-comment-prefix "* "))

;;-- C mode common hooks --;;

;; I prefer the '*' character for the block comment decorators in
;; languages with C-like, multi-line block comments.
(add-hook 'before-change-hook 
	  '(lambda () 
	     (let ((cur-decorator block-comment-decorator))
	       (if (eq major-mode 'c-mode)
		   (set 'block-comment-decorator "*")
		 (set 'block-comment-decorator cur-decorator)))))