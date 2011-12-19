(defun insert-file-doc ()
  "Insert basic file-level documentation"
  (interactive)
  (let ((decorator-header-num 70))
    ;; The variable block-comment-start is custom-defined for all
    ;; languages. However, block-comment-end is only (custom-)defined
    ;; for languages for which the standard variable
    ;; comment-multi-line is non-nil
    (let ((doc-header 
 	   (concat block-comment-start
 		   (make-string decorator-header-num 
 				(string-to-char block-comment-decorator)))))
      (let ((doc-footer (if (not (eq comment-multi-line nil))
			    (concat 
			     (make-string 
			      decorator-header-num 
			      (string-to-char block-comment-decorator))
			     block-comment-end)
			  doc-header))
	    (doc-line-start (if (not (eq comment-multi-line nil))
				(concat block-comment-decorator " ")
			      (concat block-comment-start " "))))
	(progn
	  ;; Move the point to the beginning of the buffer first so we
	  ;; don't put the documentation in a weird place
	  (goto-char (point-min))
	  
	  ;; Insert the header
	  (insert doc-header "\n")
	  ;; Insert the file information 
	  (insert doc-line-start "File " (buffer-name) "\n")
	  (indent-for-tab-command)
	  (insert doc-line-start "Written by " (user-full-name)  "\n")
	  (indent-for-tab-command)
	  (insert doc-line-start "\n")
	  (indent-for-tab-command)
	  (let ((date (if (boundp 'custom-date-format)
			  (format-time-string custom-date-format)
			(current-time-string))))
	    (insert doc-line-start "Created " date  "\n")
	    (indent-for-tab-command)
	    (insert doc-line-start "Last modified " date  "\n")
	    (indent-for-tab-command))
	  (insert doc-line-start "\n")
	  (indent-for-tab-command)
	  (insert doc-line-start "\n")
	  (indent-for-tab-command)
	  ;; Insert the footer
	  (insert doc-footer)
	  (indent-for-tab-command))))))
