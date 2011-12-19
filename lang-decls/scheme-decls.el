;;-- Load files --;;

;; Use the load-file-list function from helpers.el, assumed to be in
;; the load-path
(load "helpers.el")

;; Additional Scheme declaration files
(setq scheme-decls '("scheme-format.el"))

(let ((scheme-decls-dir (convert-standard-filename "scheme-decls/")))
  (load-file-list scheme-decls scheme-decls-dir))

;; Use petite as the default Scheme program
(custom-set-variables '(scheme-program-name "petite"))
