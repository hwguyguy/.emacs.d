(defun my-c/c-indent-new-comment-line()
  "Add new comment line and auto close block comment."
  (interactive)
  (let (star single col first-line needs-close)
    (save-excursion
      (back-to-indentation)
      (cond
       ((looking-at "\\*[^/]")
        (setq star t
              col (current-column)))
       ((looking-at "/\\*")
        (setq star t
              first-line t
              col (1+ (current-column))))
       ((looking-at "//")
        (setq single t
              col (current-column)))))
    (setq needs-close
          (and first-line
               (eolp)
               (save-excursion
                 (skip-chars-forward " \t\r\n")
                 (not (eq (char-after) ?*)))))
    (delete-horizontal-space)
    (insert "\n")
    (cond
     (star
      (indent-to col)
      (insert "* ")
      (if (and first-line needs-close)
          (save-excursion
            (insert "\n")
            (indent-to col)
            (insert "*/"))))
     (single
      (indent-to col)
      (insert "// ")))))
