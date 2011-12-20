;;----------------------------------------------------------------------
;; File file-doc.el
;; Written by Chris Frisz
;; 
;; Created 17 Dec 2011
;; Last modified 20 Dec 2011
;; 
;; file-doc.el contains the function insert-file-doc and its helpers
;; for inserting basic file-level documentation into any given file.
;;----------------------------------------------------------------------

;; Requires "insert-line" and "sub1" from helpers.el
(load "helpers.el")

(defun get-decorator (cur-mode)
  "Returns the string representing the block comment decorator based
  on the current major mode."
  (cond
    ((eq cur-mode 'c-mode) "*")
    (t "-")))

(defun get-comment-start (&optional cur-mode multi-line)
  "Return the start-of-comment string based on the multi-line
boolean argument value"
(let ((cur-mode (or cur-mode major-mode))
      (multi-line (or multi-line comment-multi-line)))
  (cond
   ;; Check that there aren't any special comment-start values for the
   ;; current mode
   ((get-comment-start-special cur-mode))
   (multi-line (substring comment-start 0 
			  (sub1 (length comment-start))))
   ;; If there's no multi-line comments, the comment-start value
   ;; will do just fine.
   (comment-start))))

(defun get-comment-start-special (&optional cur-mode)
  "Get the comment-start value for some modes for which I have
particular tastes."
  (let ((cur-mode (or cur-mode major-mode)))
    (cond
     ((or (eq cur-mode 'emacs-lisp-mode)
	  (eq cur-mode 'scheme-mode))
      ";;"))))


(defun get-comment-end (&optional multi-line)
  "Return the end-of-comment string string based on the multi-line
  boolean argument value."
  (let ((multi-line (or multi-line comment-multi-line)))
    (if multi-line
	;; In modes with comment-multi-line, comment-end has a leading
	;; space. We don't want that.
	(substring comment-end 1 (length comment-end))
	;; If there's no multi-line comments, we return nil
	nil)))

(defun get-doc-header (&optional cur-mode multi-line decor-len)
  "Returns a string representing the file documentation header based
  on the comment-start and decorator values for this mode."
  (let ((multi-line (or multi-line comment-multi-line))
	(decor-len (or decor-len 70))
	(decor (get-decorator cur-mode)))
    (let ((comment-start (get-comment-start multi-line))
	  (decor-sym (string-to-char decor)))
      (let ((decor-cnt (/ decor-len (length decor))))
	(let ((header-decor (make-string decor-cnt decor-sym)))
	  (concat comment-start header-decor))))))

(defun get-doc-footer (&optional cur-mode multi-line decor-len)
  "Returns a string representing the file documentation footer based
  on the mode, comment-multi-line, and decoration length arguments."
  (let ((cur-mode (or cur-mode major-mode))
	(multi-line (or multi-line comment-multi-line))
	(decor-len (or decor-len 70)))
    (let ((comment-end (get-comment-end multi-line)))
      (if comment-end
          (let ((decor (get-decorator cur-mode)))
            (let ((decor-sym (string-to-char decor)))
              (let ((decor-cnt (/ decor-len (length decor))))
                (let ((footer-decor (make-string decor-cnt
                                                 decor-sym)))
                  (concat footer-decor comment-end)))))
	  (get-doc-header cur-mode multi-line decor-len)))))


(defun get-file-doc-line-start (&optional multi-line cur-mode)
  "Returns the file documentation line start symbol based on the
  cur-mode argument."
  (let ((multi-line (or multi-line comment-multi-line))
	(cur-mode (or cur-mode major-mode)))
    (let ((line-start (if multi-line
			  (get-decorator cur-mode)
			  (get-comment-start multi-line))))
      (concat line-start " "))))

(defun insert-doc-line (&rest line)
  "Inserts a file documentation line starting with the
  file-doc-line-start value. Otherwise it works similarly to
  insert-line defined in helpers.el"
  (let ((cur-mode major-mode))
    (let ((doc-line-start 
	   (get-file-doc-line-start comment-multi-line cur-mode)))
      (progn
	(apply 'insert (cons doc-line-start line))
	;; We want to make sure that indentation is done properly, but
	;; this has weird results for text mode.
	(when (indent-doc-line? cur-mode)
	  (indent-for-tab-command))
	(insert "\n")))))

(defun indent-doc-line? (&optional cur-mode)
  "Returns whether or not to indent a file documentation line as
a boolean value."
  (let ((cur-mode (or cur-mode major-mode)))
    (cond
     ((eq cur-mode 'text-mode) nil)
     (t))))

(defun get-date (&optional date-format)
  (if date-format
      (format-time-string date-format)
      (current-time-string)))
  
(defun insert-file-doc (&optional decorator dec-seq-len)
  "Inserts basic file-level documentation"
  (interactive)
  (let ((doc-header (get-doc-header major-mode comment-multi-line 70))
	(doc-footer (get-doc-footer major-mode comment-multi-line 70))
	(doc-line-start 
	 (get-file-doc-line-start comment-multi-line major-mode)))
    (let ((date (get-date (when (boundp 'cjfrisz-date-format)
			    cjfrisz-date-format))))
      (progn
	    ;; Move the point to the beginning of the buffer first so we
	    ;; don't put the documentation in a weird place
	    (goto-char (point-min))
	    
	    ;; Insert the header
	    (insert-line doc-header)
	    
	    ;; Insert the file information
	    (insert-doc-line "File " (buffer-name))
	    (insert-doc-line "Written by " (user-full-name))
	    (insert-doc-line)
	    (insert-doc-line "Created " date)
	    (insert-doc-line "Last modified " date)
	    (insert-doc-line)
	    (insert-doc-line)
	    
	    ;; Insert the footer
	    (insert doc-footer)
	    ;; To get the last line indented correctly in certain
	    ;; modes (i.e. C), we have to explicitly indent here when
	    ;; necessary.
	    (and (indent-doc-line? major-mode)
	      (indent-for-tab-command))
	    ;; Final newlines
	    (insert "\n\n")))))
  