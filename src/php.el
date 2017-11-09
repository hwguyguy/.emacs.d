(defun my-php/insert-object-operator ()
  "Insert an object operator."
  (interactive)
  (insert "->"))

(defun my-php/insert-double-arrow-operator ()
  "Insert a double arrow operator."
  (interactive)
  (insert " => "))

(defun my-php/insert-pseudo-variable-this ()
  "Insert $this."
  (interactive)
  (insert "$this->"))

(defun my-php/insert-semicolon-close-statement ()
  "Jump to the end of line and insert a semicolon."
  (interactive)
  (move-end-of-line nil)
  (insert ";"))
