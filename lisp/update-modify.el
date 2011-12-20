(defun update-modify ()
  "Updates the modified date according to my standard format."
  (let ((modify-regexp 
	 "Last modified [[:digit:]]+ [[:alpha:]]+ [[:digit:]]*")
	(date (if (boundp 'cjfrisz-date-format)
		  (format-time-string cjfrisz-date-format)
		(current-time-string)))
	(start-point (point)))
    (progn
      ;; Move to the beginning of the buffer, just for sanity's sake
      (goto-char (point-min))
      ;; Find the modified string
      (when (re-search-forward modify-regexp nil t)
        ;; Replace the modified date with the current one
        (replace-match (concat "Last modified " date)))
      ;; Go back to the original point
      (goto-char start-point))))
    
