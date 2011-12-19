;; Load additional declaration files
(let ((scheme-load-path (concat user-emacs-directory
				(convert-standard-filename "lang/")
				(convert-standard-filename "scheme/"))))
  (let ((scheme-format-file 
	 (concat scheme-load-path
		 (convert-standard-filename "scheme-format.el"))))
      ;; Custom formatting for (sometimes non-standard) forms
      (load-file scheme-format-file)))

;; Use petite as the default Scheme program
(custom-set-variables '(scheme-program-name "petite"))

;;-- Scheme mode hooks --;;

;; Caps for the start of block comments
;; (add-hook 'scheme-mode-hook '(lambda () (set 'block-comment-start ";;")))
