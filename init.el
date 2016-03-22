;; package manager
(require 'package)
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package use-package-chords
  :config (key-chord-mode 1))

;; package
(use-package chords
  :chords ("ää" . prelude-duplicate-current-line-or-region))

(use-package magit
  :ensure t)

(use-package winner
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :bind (("M-D" . projectile-find-file-dwim)))

(use-package helm
  :ensure
  :init
  (setq helm-locate-command "mdfind -name %s %s")
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-for-files)))

(use-package helm-projectile
  :ensure
  :init
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))

(use-package ace-jump-mode
  :ensure
  :chords ("jj" . ace-jump-mode))

(use-package helm-swoop
  :ensure
  :bind (("M-o" . helm-swoop)))

(use-package smartparens
  :ensure
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package ensime
  :ensure t
  :commands ensime ensime-mode
  :config
  (add-hook 'scala-mode-hook 'ensime-mode)
  (add-hook 'scala-mode-hook 'linum-mode)
  :bind (("M-ö" . ensime-company)))

(use-package expand-region
  :ensure
  :bind (("M-u" . er/expand-region)
	 ("M-i" . er/contract-region)))

(use-package pbcopy
  :ensure
  :init (turn-on-pbcopy))


(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package neotree
  :ensure
  :config
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :bind (
	 ("C-c n" . neotree-toggle)))

(use-package popup-imenu
  :ensure
  :bind (("M-s" . popup-imenu)))

(use-package resize-window
  :ensure
  :bind (("C-c r" .  resize-window)))

(use-package company
  :ensure
  :config
  (global-company-mode t)
  :bind (("M-ö" . company-search-candidates)))

(use-package ace-window
  :ensure
  :chords ("kk" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package highlight-symbol
  :ensure
  :config (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package back-button
  :ensure
  :config
  (global-set-key (kbd "ESC <left>") 'back-button-local-backward)
  (global-set-key (kbd "ESC <right>") 'back-button-local-forward)
  (global-set-key (kbd "ESC <up>") 'back-button-global-forward)
  (global-set-key (kbd "ESC <down>") 'back-button-global-backward))

					; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

(global-set-key (kbd "M-j") 'hippie-expand)

(setq sentence-end-double-space nil)

(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)

(global-set-key (kbd "M-i") 'indent-region-or-buffer)

(global-set-key (kbd "M-/") 'delete-horizontal-space)

(bind-chord "zz" 'delete-indentation)

(bind-chord "kk" 'other-window)

(bind-chord "xx" 'delete-window)

(bind-chord "bb" 'helm-buffers-list)

(setq linum-format "%7d   ")

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-l") 'smart-open-line)


(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-L") 'smart-open-line-above)


(defun prelude-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
	  (if (eq major-mode 'dired-mode)
	      (dired-get-file-for-visit)
	    buffer-file-name))
	 (open (pcase system-type
		 (`darwin "open")
		 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
	 (program (if (or arg (not open))
		      (read-shell-command "Open current file with: ")
		    open)))
    (start-process "prelude-open-with-process" nil program current-file-name)))

(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
	       (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
	       (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
	(goto-char end)
	(newline)
	(insert region)
	(setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
	(exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
	(exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load themes after they are marked as safe
(load-theme 'zenburn)
(load-theme 'smart-mode-line-light)

(use-package smart-mode-line
  :ensure t
  :defer t
  :init (smart-mode-line-enable t))
