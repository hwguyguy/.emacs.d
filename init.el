(require 'cl)

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix
      system-time-locale "C")

(xterm-mouse-mode 1)
(unless (display-graphic-p)
  (menu-bar-mode 0))
(when (display-graphic-p)
  (tool-bar-mode 0))
(column-number-mode 1)

(setq make-backup-files nil); stop creating those backup~ files
(setq auto-save-default nil); stop creating those #auto-save# files
(setq backup-by-copying t); Stop emacs backup changing original file creation date

(show-paren-mode t)
(setq show-paren-style 'parenthese); do not blink to started brace
(blink-cursor-mode (- (*) (*) (*))); stop blinking cursor

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t
      initial-scratch-message nil
      ;confirm-kill-emacs 'y-or-n-p
      frame-title-format "%f - Emacs"
      echo-keystrokes 0.1
      require-final-newline t
      backward-delete-char-untabify-method nil
      mouse-wheel-progressive-speed nil
      focus-follows-mouse t
      mouse-autoselect-window t
      scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        ns-pop-up-frames nil))

(defun my-align-by-space (orig-fun &rest args)
  (let ((indent-tabs-mode nil))
    (apply orig-fun args)))
(advice-add 'align :around 'my-align-by-space)
(advice-add 'align-regexp :around 'my-align-by-space)

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))
(when (display-graphic-p)
  (set-face-attribute 'default nil :font (font-candidate '"Monaco-11:weight=normal" "Consolas-12:weight=normal" "Inconsolata-13:weight=normal" "Ubuntu Mono-13:weight=normal" "DejaVu Sans Mono-11:weight=normal" "Courier New-12:weight=normal"))
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :font (font-candidate '"Monaco-15:weight=normal"))))

(desktop-save-mode 1)

(defun copy-all()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun pwd-yank ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun my-server-start ()
  (interactive)
  (server-start)
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

;;; backup file if it is edited from winscp
;; (defun my-winscp-backup()
;;   (let ((filename (buffer-file-name))
;;         (cache-dir-prefix "c:/app/WinSCP/cache/scp[0-9]*/")
;;         (bak-dir-prefix "c:/app/WinSCP/bak/"))
;;     (when (and filename
;;                (string-match cache-dir-prefix filename))
;;       (let ((new-filename (replace-regexp-in-string cache-dir-prefix bak-dir-prefix filename)))
;;         (make-directory (file-name-directory new-filename) t)
;;         (write-region (point-min) (point-max) new-filename nil nil nil nil)))))
;; (add-hook 'after-save-hook 'my-winscp-backup)

(defmacro def-read-only-file (fun-name dir)
  `(defun ,(intern fun-name) ()
     (let ((filename (buffer-file-name)))
       (when (and filename
                  (string-match ,dir filename))
         (read-only-mode)))))

(defun my-helm-find-files-expand-directory-or-open-file()
  (interactive)
  (if (file-directory-p (helm-get-selection))
      (helm-execute-persistent-action)
    (helm-maybe-exit-minibuffer)))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun my-c-indent-new-comment-line()
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

(defun my-php-object-operator-shortcut ()
  "Insert an object operator."
  (interactive)
  (insert "->"))

(defun my-php-double-arrow-operator-shortcut ()
  "Insert a double arrow operator."
  (interactive)
  (insert " => "))

(require 'package)
(setq package-list
      '(evil
        evil-numbers
        anzu
        evil-anzu
        helm
        key-chord
        ace-jump-mode
        paredit
        auto-complete
        yasnippet
        emmet-mode
        multi-term
        flycheck
        bookmark+
        projectile
        helm-projectile
        fiplr
        rainbow-mode
        diminish
        org
        clojure-mode
        js2-mode
        php-mode
        web-mode
        scss-mode
        apache-mode
        yaml-mode))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(package-initialize)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'whitespace)
(delete 'lines whitespace-style)

(require 'anzu)

(require 'evil)
(evil-set-toggle-key "C-x C-z")
(setq-default evil-symbol-word-search t)
(delete 'help-mode evil-motion-state-modes)
(evil-mode 1)

(require 'evil-numbers)

(require 'evil-anzu)

(add-to-list 'load-path "~/.emacs.d/evil-plugins")
(require 'evil-little-word)

(require 'helm-config)
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-imenu-fuzzy-match t)
(helm-mode 1)
(define-key helm-find-files-map (kbd "<RET>") 'my-helm-find-files-expand-directory-or-open-file)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

(require 'ace-jump-mode)

(require 'paredit)
(define-key paredit-mode-map (kbd "M-;") nil)

(require 'auto-complete-config)
(defun ac-expand-common ())
(ac-config-default)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map [tab] 'ac-complete)
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map [return] nil)
(define-key ac-completing-map "\M-n" nil)
(define-key ac-completing-map "\M-p" nil)
(define-key ac-completing-map (kbd "C-n") 'ac-expand)
(define-key ac-completing-map (kbd "C-p") 'ac-expand-previous)
(define-key ac-completing-map (kbd "C-s") 'ac-isearch)
(setq ac-auto-start 1
      ac-delay 0.1
      ac-auto-show-menu 0.1
      ac-disable-faces nil)
(setq-default ac-sources
              (append '(ac-source-filename
                        ac-source-files-in-current-dir)
                      ac-sources
                      '(ac-source-yasnippet)))
(global-auto-complete-mode t)

(require 'yasnippet)
(setq yas-snippet-dirs
      `(,(concat user-emacs-directory "snippets")))
(define-key yas-keymap [(tab)] nil)
(define-key yas-keymap (kbd "TAB") nil)
(define-key yas-keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "M-p") 'yas-prev-field)
(yas-global-mode 1)

(defun my-snippet-mode-config()
  (setq require-final-newline nil))
(add-hook 'snippet-mode-hook 'my-snippet-mode-config)
(add-hook 'snippet-mode-hook 'whitespace-mode)

(require 'emmet-mode)
(let ((tbl (gethash "aliases" (gethash "html" emmet-snippets))))
  (puthash "js" "script[type=text/javascript]" tbl)
  (puthash "js:src" "script[type=text/javascript src]" tbl))
(let ((tbl (gethash "snippets" (gethash "html" emmet-snippets))))
  (puthash "fe" "<?php foreach (${child} as ): ?>" tbl)
  (puthash "ef" "<?php endforeach; ?>" tbl)
  (puthash "if" "<?php if (${child}): ?>" tbl)
  (puthash "elf" "<?php elseif (${child}): ?>" tbl)
  (puthash "el" "<?php else: ?>" tbl)
  (puthash "fi" "<?php endif; ?>" tbl))

(unless (eq system-type 'windows-nt)
  (require 'multi-term)
  (setq multi-term-program "/bin/bash"))

(require 'flycheck)

(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
(require 'bookmark+)
(setq bmkp-last-as-first-bookmark-file nil
      bmkp-bmenu-state-file (concat user-emacs-directory ".emacs-bmk-bmenu-state.el"))

(require 'projectile)
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'fiplr)

(require 'rainbow-mode)

(require 'diminish)
(eval-after-load 'whitespace
  '(diminish 'whitespace-mode))
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))
(eval-after-load 'abbrev
  '(diminish 'abbrev-mode "Ab"))
(eval-after-load 'helm
  '(diminish 'helm-mode " H"))
(eval-after-load 'paredit
  '(diminish 'paredit-mode "Par"))
(eval-after-load 'yasnippet
  '(diminish 'yas-minor-mode "ys"))

(defun my-emacs-lisp-mode-config()
  (setq indent-tabs-mode nil))
(define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp)
(define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-config)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-indent-mode)

(setq org-loop-over-headlines-in-active-region t
      org-log-done 'time
      org-startup-folded 'showeverything)
(add-hook 'org-mode-hook 'auto-complete-mode)
(add-hook 'org-mode-hook 'rainbow-mode)

(require 'cc-mode)
(define-key c-mode-base-map (kbd "M-j") 'my-c-indent-new-comment-line)

(require 'clojure-mode)
(defun my-clojure-mode-config()
  (setq indent-tabs-mode nil))
(add-hook 'clojure-mode-hook 'my-clojure-mode-config)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'electric-indent-mode)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(defun my-js2-mode-config()
  (setq indent-tabs-mode t
        tab-width 4
        js2-basic-offset 4))
(add-hook 'js2-mode-hook 'my-js2-mode-config)
(add-hook 'js2-mode-hook 'electric-pair-mode)
(add-hook 'js2-mode-hook 'electric-indent-mode)

(require 'php-mode)
(c-add-style
 "hwguyguy-php"
 '("php"
   (c-basic-offset . 4)
   (c-offsets-alist . ((statement-cont . (first php-lineup-cascaded-calls +))))))
(defun my-php-mode-config()
  (setq indent-tabs-mode t)
  (c-set-style "hwguyguy-php"))
(key-chord-define php-mode-map ",." 'my-php-object-operator-shortcut)
(key-chord-define php-mode-map ",," 'my-php-double-arrow-operator-shortcut)
(add-hook 'php-mode-hook 'my-php-mode-config)
(add-hook 'php-mode-hook 'electric-pair-mode)
(add-hook 'php-mode-hook 'electric-indent-mode)
(add-hook 'php-mode-hook 'flycheck-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '(".*\/views\/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-config()
  (setq indent-tabs-mode t
        tab-width 4
        web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-style-padding 0
        web-mode-script-padding 4)
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#93a1a1")
  (modify-syntax-entry ?_    "_" web-mode-syntax-table))
(add-hook 'web-mode-hook 'my-web-mode-config)
(add-hook 'web-mode-hook 'electric-pair-mode)
(add-hook 'web-mode-hook 'auto-complete-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(key-chord-define web-mode-map ",." 'my-php-object-operator-shortcut)
(key-chord-define web-mode-map ",," 'my-php-double-arrow-operator-shortcut)

(defun my-css-mode-config()
  (setq tab-width 4))
(add-hook 'css-mode-hook 'my-css-mode-config)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'electric-pair-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(add-hook 'scss-mode-hook 'electric-pair-mode)
(add-hook 'scss-mode-hook 'electric-indent-mode)
(add-hook 'scss-mode-hook 'emmet-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)

(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
(define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
(define-key ibuffer-mode-map (kbd "J") 'ibuffer-jump-to-buffer)
(define-key ibuffer-mode-map (kbd "K") 'ibuffer-do-kill-lines)
(add-hook 'ibuffer-mode-hook 'hl-line-mode)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c h") 'helm-imenu)
(global-set-key (kbd "M-s") 'ace-jump-char-mode)
(global-set-key (kbd "C-c C-k") 'ace-jump-word-mode)
(global-set-key (kbd "C-c C-l") 'ace-jump-line-mode)

(define-key evil-motion-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-motion-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-motion-state-map (kbd "e") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "ge") 'evil-backward-little-word-end)

(define-key evil-normal-state-map (kbd "M-SPC") 'keyboard-quit)
(define-key evil-normal-state-map [f7] 'split-window-horizontally)
(define-key evil-normal-state-map [f8] 'split-window-vertically)
(define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'help-command)
(define-key evil-normal-state-map (kbd "M-w") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "M-r") 'ace-jump-line-mode)
(define-key evil-normal-state-map " bb" 'helm-mini)
(define-key evil-normal-state-map " ff" 'helm-find-files)
(define-key evil-normal-state-map " be" 'ibuffer)
(define-key evil-normal-state-map " fd" 'fiplr-find-file)
(define-key evil-normal-state-map " fp" 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)

(define-key evil-visual-state-map (kbd "C-g") 'evil-exit-visual-state)
(define-key evil-visual-state-map (kbd "M-SPC") 'evil-exit-visual-state)
(define-key evil-visual-state-map (kbd "M-w") 'ace-jump-word-mode)
(define-key evil-visual-state-map (kbd "M-r") 'ace-jump-line-mode)

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "M-SPC") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-v") 'quoted-insert)
(define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
(define-key evil-insert-state-map (kbd "M-h") 'backward-kill-word)
(define-key evil-insert-state-map (kbd "C-n") 'ac-start)
(define-key evil-insert-state-map (kbd "C-p") 'ac-start)
(define-key evil-insert-state-map (kbd "M-w") 'ace-jump-word-mode)
(define-key evil-insert-state-map (kbd "M-r") 'ace-jump-line-mode)

(evil-make-intercept-map paredit-mode-map)

(let ((override (concat user-emacs-directory "my-init/override.el")))
  (when (file-exists-p override)
    (load override)))
