(defun empty? (e)
  "A simple predicate to test if the argument is the empty list."
  (eq e ()))

(defun load-file-list (file-list &optional path)
  "Load a list of files either as fully-qualified paths or files
in directories included in the load-path. Optionally, a path can
be provided, assumed to be the path for all files in file-list."
  (while (not (empty? file-list))
    (let* ((file (car file-list))
	   (file-path (concat path 
			      (convert-standard-filename file))))
      (progn
	(load file-path)
	(setq file-list (cdr file-list))))))

(defun insert-line (&rest line)
  "Insert a line followed by a newline character and moves the
point according to the tab command."
  (apply 'insert (append line '("\n"))))

(defun add1 (n)
  "Increment an integer value by one."
  (+ n 1))

(defun sub1 (n)
  "Decrement an integer value by one."
  (- n 1))
