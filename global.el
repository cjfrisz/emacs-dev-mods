;; Shortcut for goto-line
(global-set-key "\M-g" 'goto-line)

;; I find narrow-to-region useful
(put 'narrow-to-region 'disabled nil)

;; For block comments, I like to have a repeated character at the
;; beginning, end, and often throughout blocks. By default, I use the
;; dash ('-') character. I prefer different ones for certain
;; languages, and so there are hooks for those major modes.
(set 'block-comment-decorator "-")

;; Enable column number mode
(column-number-mode t)

;; And...let's show the time
(display-time)

;; Date format string
(set 'custom-date-format "%e %b %Y")

;; Default to text mode instead of Fundamental
(setq default-major-mode 'text-mode)

;; Set fill-column value explicitly
(set 'fill-column 70)

;; Turn on auto-fill-mode for text mode
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)