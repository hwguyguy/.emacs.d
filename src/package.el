(defun my-package/package-install-refresh-contents-once (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'my-package/package-install-refresh-contents-once))
