;;----------------------------------------------------------------------
;; File update-modify.el
;; Written by Chris Frisz
;; 
;; Created 17 Dec 2011
;; Last modified  8 Jan 2012
;; 
;; Defines a function, update-modify, to update the modification date
;; according to the standard format used by insert-file-doc. This can
;; be used as a hook with the before-save-hook variable.
;; ----------------------------------------------------------------------

(defun update-modify ()
  "Updates the modified date according to my standard format."
  (let ((modify-regexp 
	 "Last modified[[:space:]]+[[:digit:]]+ [[:alpha:]]+ [[:digit:]]*")
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
    
