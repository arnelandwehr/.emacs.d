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
  (add-hook 'ensime-mode-hook (lambda () (setq-local imenu-generic-expression '(("test" "^.*\\(it.*\\).*" 1)))))
  (add-hook 'ensime-mode-hook (lambda () (setq-local imenu-create-index-function (lambda () (append (imenu-default-create-index-function) (ensime-imenu-index-function))))))
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

;; hippie expand is dabbrev expand on steroids
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

(global-set-key (kbd "M-I") 'indent-region-or-buffer)

(global-set-key (kbd "M-/") 'delete-horizontal-space)

(bind-chord "zz" 'delete-indentation)

(bind-chord "kk" 'ace-window)

(bind-chord "xx" 'delete-window)

(bind-chord "bb" 'helm-buffers-list)

(setq linum-format "%7d   ")

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(global-set-key (kbd "M-e") 'helm-projectile-find-file)

(global-set-key (kbd "M--") 'comment-or-uncomment-region)

(bind-chord "ää" 'prelude-duplicate-current-line-or-region)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(fci-rule-color "#3E3D31")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
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

