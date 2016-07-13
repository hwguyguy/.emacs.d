(require 'cl)

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix
      system-time-locale "C")

(setq make-backup-files nil); stop creating those backup~ files
(setq auto-save-default nil); stop creating those #auto-save# files
(setq backup-by-copying t); Stop emacs backup changing original file creation date

(xterm-mouse-mode 1)
(unless (display-graphic-p)
  (menu-bar-mode 0))
(when (display-graphic-p)
  (tool-bar-mode 0))
(column-number-mode 1)

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
(setq-default indent-tabs-mode t
              tab-width 4
              c-basic-offset 4)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-region 'disabled nil)

(unless (display-graphic-p)
  (set-display-table-slot standard-display-table 'wrap ?\273))

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

(defun executable-find-first-occurrence (&rest bins)
  (if (executable-find (car bins))
      (car bins)
    (apply 'executable-find-first-occurrence (cdr bins))))

(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(defun copy-all ()
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

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

(defun load-theme-no-confirm (theme)
  (interactive
   (list
    (intern (completing-read "Theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (load-theme theme t))

(defun my-server-start ()
  (interactive)
  (server-start)
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 2 1024 1024))
    ;; (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
;; (add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

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

(defmacro def-read-only-file (fun-name path)
  `(defun ,(intern fun-name) ()
     (let ((filename (buffer-file-name)))
       (when (and filename
                  (string-match ,path filename))
         (read-only-mode)))))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

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

(defun my-php-pseudo-variable-this-shortcut ()
  "Insert $this."
  (interactive)
  (insert "$this->"))

(defun my-semicolon-close-statement-shortcut ()
  "Jump to the end of line and insert a semicolon."
  (interactive)
  (move-end-of-line nil)
  (insert ";"))

(defalias 'bc 'kill-this-buffer)
(defalias 'ca 'copy-all)
(defalias 'dv 'describe-variable)
(defalias 'dk 'describe-key)
(defalias 'df 'describe-function)
(defalias 'tt 'multi-term)

(require 'package)
(setq my-packages
      '(
        undo-tree
        evil
        evil-numbers
        anzu
        evil-anzu
        helm
        key-chord
        ace-jump-mode
        paredit
        fuzzy
        auto-complete
        yasnippet
        emmet-mode
        multi-term
        flycheck
        bookmark+
        projectile
        helm-projectile
        fiplr
        sr-speedbar
        rainbow-mode
        diminish
        org
        clojure-mode
        clj-refactor
        js2-mode
        ;; ac-js2
        ruby-end
        rinari
        php-mode
        web-mode
        scss-mode
        tss
        apache-mode
        nginx-mode
        yaml-mode
        ))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
(package-initialize)
(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(require 'whitespace)
(delete 'lines whitespace-style)

(electric-pair-mode 1)
(electric-indent-mode 1)

(desktop-save-mode 1)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)

(global-subword-mode)

(require 'undo-tree)
(defun undo-in-term-mode ()
  (interactive)
  (when (and (fboundp 'my-term-send-undo)
             (string-equal major-mode "term-mode")
             (term-in-char-mode))
    (my-term-send-undo)
    t))
(advice-add 'undo-tree-undo :before-until 'undo-in-term-mode)

(require 'evil)
(defun translate-ctrl-h-to-del-in-evil-insert-state (prompt)
  (if (and (bound-and-true-p evil-mode)
           (eq evil-state 'insert))
      (kbd "DEL")
    (kbd "C-h")))
(evil-set-toggle-key "C-x C-z")
(setq-default evil-symbol-word-search t)
(delete 'help-mode evil-motion-state-modes)
(delete 'eshell-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'eshell-mode)
(delete 'term-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'term-mode)
(evil-mode 1)

(require 'evil-numbers)

(require 'anzu)

(require 'evil-anzu)

(add-to-list 'load-path "~/.emacs.d/evil-plugins")
(require 'evil-little-word)

(require 'helm-config)
(defun helm-find-files-expand-directory-or-open-file ()
  (interactive)
  (if (file-directory-p (helm-get-selection))
      (helm-execute-persistent-action)
    (helm-maybe-exit-minibuffer)))
(defun helm-find-files-insert-current-directory ()
  (interactive)
  (kill-whole-line)
  (insert "/./"))
(defun helm-buffer-run-kill-buffers-persistent ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'kill-action '(helm-kill-marked-buffers . never-split))
    (helm-execute-persistent-action 'kill-action)
    (helm-force-update)))
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-imenu-fuzzy-match t
      helm-ff-newfile-prompt-p nil)
(helm-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "M-h") 'backward-kill-word)
(define-key helm-find-files-map (kbd "<RET>") 'helm-find-files-expand-directory-or-open-file)
(define-key helm-find-files-map (kbd "C-r") 'helm-find-files-insert-current-directory)
(let ((helm-find-files-C-h-map (lookup-key helm-find-files-map (kbd "C-h"))))
  ;; make sure C-h is no longer a prefix key
  (define-key helm-find-files-map (kbd "C-h") nil)
  ;; rebind "C-h ..." to "M-m ..." to preserve functionality
  (define-key helm-find-files-map (kbd "M-m") helm-find-files-C-h-map))
(define-key helm-buffer-map (kbd "M-D" ) 'helm-buffer-run-kill-buffers-persistent)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

(require 'ace-jump-mode)

(require 'paredit)
(define-key paredit-mode-map (kbd "M-;") nil)
(define-key paredit-mode-map ";" nil)

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
(define-key ac-completing-map (kbd "M-n") 'ac-expand)
(define-key ac-completing-map (kbd "M-p") 'ac-expand-previous)
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
(define-key yas-keymap (kbd "M-l") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "M-S-l") 'yas-prev-field)
(define-key yas-keymap (kbd "M-L") 'yas-prev-field)
(define-key ac-completing-map (kbd "M-l") 'yas-next-field-or-maybe-expand)
(define-key ac-completing-map (kbd "M-S-l") 'yas-prev-field)
(define-key ac-completing-map (kbd "M-L") 'yas-prev-field)
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
(let ((tbl (gethash "snippets" (gethash "css" emmet-snippets))))
  (puthash "ta" "text-align:center;" tbl))

(unless (eq system-type 'windows-nt)
  (require 'term)
  (define-key term-raw-map (kbd "M-x") 'helm-M-x)

  (require 'multi-term)
  (defun my-term-send-backward-kill-word ()
    (interactive)
    (term-send-raw-string "\e\C-?"))
  (defun my-term-send-undo ()
    (interactive)
    (term-send-raw-string "\C-_"))
  (defun my-term-send-ctrl-z ()
    (interactive)
    (term-send-raw-string "\C-z"))
  (defun my-term-send-ctrl-x ()
    (interactive)
    (term-send-raw-string "\C-x"))
  (defun my-term-send-shift-tab ()
    (interactive)
    (term-send-raw-string "\e[Z"))
  (defun my-term-toggle-mode ()
    (interactive)
    (if (term-in-line-mode)
        (term-char-mode)
      (term-line-mode)))
  (setq multi-term-program nil
        multi-term-switch-after-close nil)
  (delete "C-h" term-unbind-key-list)
  (setq term-bind-key-alist (append '(("M-h" . my-term-send-backward-kill-word)
                                      ("M-DEL" . my-term-send-backward-kill-word)
                                      ("M-d" . term-send-forward-kill-word)
                                      ("C-z" . my-term-send-ctrl-z)
                                      ("C-c C-x" . my-term-send-ctrl-x)
                                      ("<backtab>" . my-term-send-shift-tab)
                                      ("C-c C-j" . term-line-mode)
                                      ("C-c C-k" . term-char-mode))
                                    term-bind-key-alist)))

(require 'flycheck)

(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
(require 'bookmark+)
(setq bmkp-last-as-first-bookmark-file nil
      bmkp-bmenu-state-file (concat user-emacs-directory ".emacs-bmk-bmenu-state.el"))
(defun my-recenter-after-bookmark-jump (&rest r)
  (recenter))
(advice-add 'bookmark-jump :after 'my-recenter-after-bookmark-jump)

(require 'projectile)
(require 'helm-projectile)
(setq projectile-completion-system 'helm
      projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
(defun projectile-get-ext-command ()
  "Determine which external command to invoke based on the project's VCS."
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'projectile) projectile-generic-command)
     ((eq vcs 'git) projectile-git-command)
     ((eq vcs 'hg) projectile-hg-command)
     ((eq vcs 'fossil) projectile-fossil-command)
     ((eq vcs 'bzr) projectile-bzr-command)
     ((eq vcs 'darcs) projectile-darcs-command)
     ((eq vcs 'svn) projectile-svn-command)
     (t projectile-generic-command))))
(defun projectile-project-vcs (&optional project-root)
  "Determine the VCS used by the project if any.
PROJECT-ROOT is the targeted directory.  If nil, use
`projectile-project-root'."
  (or project-root (setq project-root (projectile-project-root)))
  (cond
   ((projectile-file-exists-p (expand-file-name ".projectile" project-root)) 'projectile)
   ((projectile-file-exists-p (expand-file-name ".git" project-root)) 'git)
   ((projectile-file-exists-p (expand-file-name ".hg" project-root)) 'hg)
   ((projectile-file-exists-p (expand-file-name ".fossil" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
   ((projectile-file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
   ((projectile-file-exists-p (expand-file-name ".svn" project-root)) 'svn)
   ((projectile-locate-dominating-file project-root ".git") 'git)
   ((projectile-locate-dominating-file project-root ".hg") 'hg)
   ((projectile-locate-dominating-file project-root ".fossil") 'fossil)
   ((projectile-locate-dominating-file project-root ".bzr") 'bzr)
   ((projectile-locate-dominating-file project-root "_darcs") 'darcs)
   ((projectile-locate-dominating-file project-root ".svn") 'svn)
   (t 'none)))
(helm-projectile-on)
(projectile-global-mode)

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
(eval-after-load 'emmet-mode
  '(diminish 'emmet-mode "Em"))
(eval-after-load 'rainbow-mode
  '(diminish 'rainbow-mode))

(defun my-emacs-lisp-mode-config()
  (setq indent-tabs-mode nil))
(define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp)
(define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-config)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(require 'org)
(defun my-org-mode-config ()
  (setq indent-tabs-mode nil))
(setq org-loop-over-headlines-in-active-region t
      org-log-done 'time
      org-startup-folded 'showeverything
      org-src-fontify-natively t)
(add-hook 'org-mode-hook 'my-org-mode-config)
(add-hook 'org-mode-hook 'auto-complete-mode)
(add-hook 'org-mode-hook 'rainbow-mode)
(define-key org-mode-map (kbd "M-h") 'backward-kill-word)

(require 'cc-mode)
(c-add-style
 "linux-tabs-only"
 '("linux"
   (c-basic-offset . 4)
   (c-offsets-alist . ((arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only)))))
(defun my-c-mode-config ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (when (and filename
               (string-match (expand-file-name "~/pg/linux-trees") filename))
               (setq indent-tabs-mode t
                     tab-width 4)
               (c-set-style "linux-tabs-only"))))
(add-hook 'c-mode-hook 'my-c-mode-config)
(define-key c-mode-base-map (kbd "M-j") 'my-c-indent-new-comment-line)

(require 'clojure-mode)
(defun my-clojure-mode-config()
  (setq indent-tabs-mode nil))
(add-hook 'clojure-mode-hook 'my-clojure-mode-config)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'clj-refactor-mode)

(require 'clj-refactor)
(cljr-add-keybindings-with-prefix "C-c C-m")

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(setq cperl-highlight-variables-indiscriminately t
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4)

(require 'js2-mode)
;; Override js2-mode indentation function
(cl-defun js2-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (when (nth 4 parse-status)
      (cl-return-from js2-proper-indentation (js2--comment-indent parse-status)))
    (let* ((at-closing-bracket (looking-at "[]})]"))
           (backward-indent-p (looking-at ")"))
           (same-indent-p (or at-closing-bracket
                              (looking-at "\\_<case\\_>[^:]")
                              (and (looking-at "\\_<default:")
                                   (save-excursion
                                     (js2-backward-sws)
                                     (not (memq (char-before) '(?, ?{)))))))
           (continued-expr-p (js2-continued-expression-p))
           (declaration-indent (and js2-pretty-multiline-declarations
                                    (js2-multiline-decl-indentation)))
           (bracket (nth 1 parse-status))
           beg indent)
      (cond
       ;; indent array comprehension continuation lines specially
       ((and bracket
             (>= js2-language-version 170)
             (not (js2-same-line bracket))
             (setq beg (js2-indent-in-array-comp parse-status))
             (>= (point) (save-excursion
                           (goto-char beg)
                           (point-at-bol)))) ; at or after first loop?
        (js2-array-comp-indentation parse-status beg))

       ((js2-ctrl-statement-indentation))

       ((and declaration-indent continued-expr-p)
        (+ declaration-indent js2-basic-offset))

       (declaration-indent)

       (bracket
        (goto-char bracket)
        (cond
         ((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
          (when (save-excursion (skip-chars-backward " \t\n)")
                                (looking-at ")"))
            (backward-list))
          (back-to-indentation)
          (js2-maybe-goto-declaration-keyword-end bracket)
          (setq indent
                (cond (same-indent-p
                       (current-column))
                      (continued-expr-p
                       (+ (current-column) (* 2 js2-basic-offset)))
                      (t
                       (+ (current-column) js2-basic-offset))))
          (if (and js2-indent-switch-body
                   (not at-closing-bracket)
                   (looking-at "\\_<switch\\_>"))
              (+ indent js2-basic-offset)
            indent))
         (t
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (when backward-indent-p
            (back-to-indentation))
          (current-column))))

       (continued-expr-p js2-basic-offset)

       (t 0)))))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(setq js2-basic-offset 4
      js2-pretty-multiline-declarations nil)
(defun my-js2-mode-config()
  (setq indent-tabs-mode t
        tab-width 4)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(add-hook 'js2-mode-hook 'my-js2-mode-config)
(defun my-js2-jsx-mode-config ()
  (make-local-variable 'js2-strict-trailing-comma-warning)
  (setq js2-strict-trailing-comma-warning nil))
(add-hook 'js2-jsx-mode-hook 'my-js2-jsx-mode-config)

;; (require 'ac-js2)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)

(require 'js)
;; Override js-mode indentation function
(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           ;;
           ;; CHANGED: In multi-lines if statement, closing paren
           ;; should be indent at the the same level as the beginning
           ;; of the opening statement, instead of the level of the
           ;; opening paren.
           (let ((same-indent-p (looking-at "[]})]"))
                 (backward-indent-p (looking-at ")"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (cond (same-indent-p
                                  (current-column))
                                 (continued-expr-p
                                  (+ (current-column) (* 2 js-indent-level)
                                     js-expr-indent-offset))
                                 (t
                                  (+ (current-column) js-indent-level
                                     (pcase (char-after (nth 1 parse-status))
                                       (?\( js-paren-indent-offset)
                                       (?\[ js-square-indent-offset)
                                       (?\{ js-curly-indent-offset)))))))
                     (if in-switch-p
                         (+ indent js-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (when backward-indent-p
                 (back-to-indentation))
               (current-column))))
          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))
(defun my-js-mode-config ()
  (setq indent-tabs-mode t
        tab-width 4
        js-indent-level 4)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (string-match "package.json" filename))
      (make-local-variable 'js-indent-level)
      (setq indent-tabs-mode nil
            tab-width 2
            js-indent-level 2))))
(add-hook 'js-mode-hook 'my-js-mode-config)

(require 'ruby-end)
(setq ruby-end-insert-newline nil)

(require 'rinari)
(global-rinari-mode 1)

(add-hook 'ruby-mode-hook 'flycheck-mode)

(require 'php-mode)
(c-add-style
 "hwguyguy-php"
 '("php"
   (c-basic-offset . 4)
   (c-offsets-alist . ((case-label . 0)
                       (statement-cont . (first php-lineup-cascaded-calls +))))))
(defun my-php-mode-config()
  (setq indent-tabs-mode t
        require-final-newline t)
  (c-set-style "hwguyguy-php"))
(add-hook 'php-mode-hook 'my-php-mode-config)
(add-hook 'php-mode-hook 'flycheck-mode)
(key-chord-define php-mode-map ",." 'my-php-object-operator-shortcut)
(key-chord-define php-mode-map ",/" 'my-php-double-arrow-operator-shortcut)
(key-chord-define php-mode-map "<>" 'my-php-pseudo-variable-this-shortcut)

(defun my-nxml-mode-config ()
  (setq indent-tabs-mode t
        tab-width 4
        nxml-child-indent 4
        nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook 'my-nxml-mode-config)
(add-hook 'nxml-mode-hook 'auto-complete-mode)

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
        tab-width 4))
(setq web-mode-markup-indent-offset 4
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-style-padding 0
      web-mode-script-padding 4
      web-mode-enable-auto-pairing t
      web-mode-enable-auto-quoting nil
      web-mode-engines-alist '(("php" . "\\.phtml\\'")))
(set-face-attribute 'web-mode-symbol-face nil :foreground "Snow4")
;; (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#93a1a1")
;; (modify-syntax-entry ?_    "_" web-mode-syntax-table)
(add-hook 'web-mode-hook 'my-web-mode-config)
(add-hook 'web-mode-hook 'auto-complete-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(key-chord-define web-mode-map ",." 'my-php-object-operator-shortcut)
(key-chord-define web-mode-map ",/" 'my-php-double-arrow-operator-shortcut)

(require 'sgml-mode)
;; XML offset for js2-jsx-mode
(setq sgml-basic-offset 4)

(defun my-css-mode-config()
  (setq tab-width 4))
(add-hook 'css-mode-hook 'my-css-mode-config)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(add-hook 'scss-mode-hook 'emmet-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'auto-complete-mode)

(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook 'auto-complete-mode)

(defun my-sh-mode-config ()
  (setq indent-tabs-mode t
        tab-width 4))
(add-hook 'sh-mode-hook 'my-sh-mode-config)

(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("/etc/apache2/sites-\\(available\\|enabled\\)/" . apache-mode))

(require 'nginx-mode)
(setq nginx-indent-tabs-mode t)
(add-to-list 'auto-mode-alist '("/etc/nginx/.*" . nginx-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
(define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
(define-key ibuffer-mode-map (kbd "J") 'ibuffer-jump-to-buffer)
(define-key ibuffer-mode-map (kbd "K") 'ibuffer-do-kill-lines)
(add-hook 'ibuffer-mode-hook 'hl-line-mode)

(defun my-extra-match (orig-fun &rest args)
  (interactive)
  (condition-case err
      (apply orig-fun args)
    (error
     (cond ((string-equal major-mode "nxml-mode")
            (nxml-token-after)
            (cond ((memq xmltok-type '(start-tag partial-start-tag))
                   (nxml-up-element)
                   (backward-sexp))
                  ((memq xmltok-type '(end-tag partial-end-tag))
                   (nxml-backward-up-element)
                   (forward-sexp))))
           ((string-equal major-mode "web-mode")
            (web-mode-navigate))))))
(advice-add 'evil-jump-item :around 'my-extra-match)

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

(define-key key-translation-map (kbd "C-h") 'translate-ctrl-h-to-del-in-evil-insert-state)

(define-key minibuffer-inactive-mode-map [mouse-1] nil)

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
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-v") 'quoted-insert)
(define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
(define-key evil-insert-state-map (kbd "M-h") 'backward-kill-word)
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
(define-key evil-insert-state-map (kbd "M-n") 'ac-start)
(define-key evil-insert-state-map (kbd "M-p") 'ac-start)
(define-key evil-insert-state-map (kbd "M-S-n") 'ac-fuzzy-complete)
(define-key evil-insert-state-map (kbd "M-w") 'ace-jump-word-mode)
(define-key evil-insert-state-map (kbd "M-r") 'ace-jump-line-mode)
(define-key evil-insert-state-map (kbd "C-l") 'yas-expand)

(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "C-d") 'delete-forward-char)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

(evil-make-intercept-map paredit-mode-map)

(let ((override (concat user-emacs-directory "init.override.el")))
  (when (file-exists-p override)
    (load override)))
