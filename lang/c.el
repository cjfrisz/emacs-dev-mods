;; Set the block comment beginning-of-line string
(custom-set-variables '(c-block-comment-prefix "* "))

;;-- C mode common hooks --;;

;; Caps for the start and end of block comments
(add-hook 'c-mode-common-hook '(lambda () (set 'block-comment-start "/*")))
(add-hook 'c-mode-common-hook '(lambda () (set 'block-comment-end "*/")))


;; I prefer the '*' character for the block comment decorators in
;; languages with C-like, multi-line block comments.
(let ((c-comment-decorator "*"))
  (add-hook 'before-change-hook 
	    '(lambda () 
	       (let ((cur-decorator block-comment-decorator))
		 (if (eq major-mode 'c-mode)
		     (set 'block-comment-decorator 
			  c-comment-decorator)
		   (set 'block-comment-decorator cur-decorator))))))