;; I never use the ";" comment form, as I neither like the way it
;; looks nor do I like how Emacs defaults to indenting it to the right
;; side. I prefer comments aligned with the current code indentation
;; and placing in-code comments above the statements they're
;; documenting. Thus, we override the Emacs default with the ";;"
;; comment form.
(add-hook 'lisp-mode-hook '(lambda () (set 'comment-start ";;")))