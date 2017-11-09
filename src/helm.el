(defun my-helm/helm-find-files-expand-directory-or-open-file ()
  (interactive)
  (if (file-directory-p (helm-get-selection))
      (helm-execute-persistent-action)
    (helm-maybe-exit-minibuffer)))

(defun my-helm/helm-find-files-insert-current-directory ()
  (interactive)
  (kill-whole-line)
  (insert "/./"))

(defun my-helm/helm-buffer-run-kill-buffers-persistent ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'kill-action '(helm-kill-marked-buffers . never-split))
    (helm-execute-persistent-action 'kill-action)
    (helm-force-update)))
