;; Requires "insert-line" from helpers.el
(load "helpers.el")

(defun insert-file-doc ()
  "Insert basic file-level documentation"
  (interactive)
  (let* ((decorator-seq-len 70)
	 (decorator-count (/ decorator-seq-len 
			     (length block-comment-decorator)))
	 (decorator (make-string 
		     decorator-seq-len 
		     (string-to-char block-comment-decorator))))
    (let* ((comment-start (if comment-multi-line
			      (substring 
			       comment-start 
			       0 
			       (- (length comment-start) 1))
			    comment-start))
	   (comment-end (if comment-multi-line
			    (substring comment-end 
				       1 
				       (length comment-end))
			  nil)))
      (let* ((doc-header (concat comment-start decorator))
	     (doc-footer (if comment-end
			     (concat decorator comment-end)
			   doc-header))
	     (doc-line-start (if comment-multi-line
				 (concat block-comment-decorator
					 " ")
			       (concat comment-start " "))))
	(let ((insert-doc-line '(lambda (&rest line)
				  (apply 'insert-line 
					 (cons doc-line-start line)))))
	  (progn
	    ;; Move the point to the beginning of the buffer first so we
	    ;; don't put the documentation in a weird place
	    (goto-char (point-min))
	    
	    ;; Insert the header
	    (insert-line doc-header)
	    
	    ;; Insert the file information
	    (funcall insert-doc-line "File " (buffer-name))
	    (funcall insert-doc-line "Written by " (user-full-name))
	    (funcall insert-doc-line)
	    (let ((date (if (boundp 'custom-date-format)
			    (format-time-string custom-date-format)
			  (current-time-string))))
	      (funcall insert-doc-line "Created " date)
	      (funcall insert-doc-line "Last modified " date))
	    (funcall insert-doc-line)
	    (funcall insert-doc-line)
	    
	    ;; Insert the footer
	    (insert-line doc-footer)))))))
  