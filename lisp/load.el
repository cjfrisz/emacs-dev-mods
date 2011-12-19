;;-- Load files --;;

(setq lisp-files '("file-doc.el"
		    "update-modify.el"))

(let ((lisp-load-path (concat user-emacs-directory
			      (convert-standard-filename "lisp/")))
      (lisp-files lisp-files)) ;; Shadowing
  (while (not (eq lisp-files ()))
    (let ((file (concat lisp-load-path
			(convert-standard-filename (car lisp-files)))))
      (load-file file)
      (setq lisp-files (cdr lisp-files)))))

;;-- Set hooks --;;

;; Set the before-save hook to update the modify date
(add-hook 'before-save-hook 'update-modify)
