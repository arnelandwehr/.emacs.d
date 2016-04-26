					; package manager
(require 'package)
(setq package-enable-at-startup nil)  ;; To prevent initialising twice
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'eldoc-mode)
(diminish 'eldoc-mode)

					; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package use-package-chords
  :config (key-chord-mode 1))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)

					; (setq url-proxy-services
					;                 '(("http"     . "localhost:8888")))
					;(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-default-font "SourceCodePro 12")

					;(powerline-default-theme)

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode)
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

(global-set-key (kbd "M-DEL") 'contextual-backspace)

(setq ring-bell-function 'ignore)

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)


(global-hl-line-mode t)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
					; disabled because of go fmt
					;  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

					; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(setq
 backup-by-copying t      ;don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ;don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ;use versioned backups

					; packages

(eval-after-load "dash" '(dash-enable-font-lock))

(use-package dash-functional
  :ensure)

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'enriched-mode)
  :ensure)

(use-package org-pomodoro
  :ensure)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind (("M-J" . helm-yas-complete)
         ("TAB" . indent-for-tab-command)))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-mode))

(use-package flycheck
  :ensure t)

(use-package helm-c-yasnippet
  :ensure t)

(use-package company-go
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package move-text
  :ensure t
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

(use-package dired-details
  :ensure t
  :config
  (dired-details-install)
  (setq-default dired-details-hidden-string ""))

(use-package magit
  :ensure t)

(use-package winner
  :ensure t
  :defer t)

(use-package abbrev
  :diminish abbrev-mode
  :config)

(use-package helm-projectile
  :ensure
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))

(use-package writeroom-mode
  :ensure
  :config
  (setq writeroom-mode-line t))

(use-package projectile
  :ensure t
  :config
  (setq projectile-switch-project-action 'projectile-vc)
  (projectile-global-mode)
  :bind (("M-D" . projectile-find-file-dwim)))

(use-package helm
  :ensure
  :diminish helm-mode
  :config
  (setq helm-locate-command "mdfind -name %s %s")
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-for-files)))

(use-package window-purpose
  :ensure)

(use-package etags-select
  :ensure t
  :commands etags-select-find-tag)

(use-package helm-gtags
  :ensure t)

(use-package ace-jump-mode
  :ensure
  :chords ("jj" . ace-jump-mode))

(use-package ggtags
  :ensure t)

(use-package zoom-window
  :ensure
  :chords ("vv" . zoom-window-zoom))

(use-package helm-swoop
  :ensure
  :config (setq helm-swoop-split-with-multiple-windows t)
  :bind (("M-o" . helm-swoop)))

(use-package cider
  :ensure)

(autoload 'cider--make-result-overlay "cider-overlays")

(use-package spaceline
  :ensure)

(require 'spaceline-config)
(spaceline-emacs-theme)

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(use-package smartparens
  :ensure
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package ensime
  :ensure t
  :commands ensime ensime-mode
  :config
  (scala-mode:goto-start-of-code)
  (add-hook 'scala-mode-hook 'ensime-mode)
  (add-hook 'scala-mode-hook 'linum-mode)
  (add-hook 'ensime-mode-hook (lambda () (setq-local imenu-generic-expression '(("test" "^ *\\(it .*\\)in *{" 1)))))
  (add-hook 'ensime-mode-hook (lambda () (setq-local imenu-create-index-function (lambda () (append (imenu-default-create-index-function) (ensime-imenu-index-function))))))
  :bind (("M-ö" . ensime-company)))

(use-package expand-region
  :ensure
  :bind (("M-u" . er/expand-region)
         ("M-i" . er/contract-region)))

(use-package pbcopy
  :ensure
  :config (turn-on-pbcopy))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (("M-z" . undo-tree-undo)
	 ("M-Z" . undo-tree-redo))
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package neotree
  :ensure
  :config
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'nerd)
  (setq neo-vc-integration nil)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :bind (("C-c n" . neotree-toggle)))

(use-package popup-imenu
  :ensure
  :bind (("M-s" . popup-imenu)))

(use-package resize-window
  :ensure
  :bind (("C-c r" .  resize-window)))

(use-package company
  :ensure
  :diminish company-mode
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  :bind (("M-ö" . company-search-candidates)))

(use-package ace-window
  :ensure
  :chords ("kk" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package highlight-symbol
  :ensure
  :diminish highlight-symbol-mode
  :config (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package powerline
  :ensure t
  :config (powerline-default-theme))

(use-package back-button
  :ensure
  :config
  (global-set-key (kbd "ESC <left>") 'back-button-local-backward)
  (global-set-key (kbd "ESC <right>") 'back-button-local-forward)
  (global-set-key (kbd "ESC <up>") 'back-button-global-forward)
  (global-set-key (kbd "ESC <down>") 'back-button-global-backward))

(use-package dumb-jump
  :ensure)

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


(global-set-key (kbd "<f2>") 'start-kbd-macro)
(global-set-key (kbd "<f3>") 'end-kbd-macro)
(global-set-key (kbd "<f4>") 'call-last-kbd-macro)


;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(global-set-key(kbd "M-j") 'hippie-expand)

(setq sentence-end-double-space nil)

(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)

(global-set-key (kbd "M-I") 'indent-region-or-buffer)

(global-set-key (kbd "M-/") 'delete-horizontal-space)

(bind-chord "zz" 'delete-indentation)

(bind-chord "kk" 'ace-window)

(bind-chord "xx" 'delete-window)

(bind-chord "bb" 'helm-buffers-list)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(delete-selection-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(global-set-key (kbd "M-e") 'helm-projectile-find-file)

(global-set-key (kbd "M-#") 'comment-or-uncomment-region)

(bind-chord "ää" 'prelude-duplicate-current-line-or-region)

(add-hook 'neotree-mode-hook 'hl-line-mode)

(set-default 'truncate-lines t)

(setq linum-format "%5d  ")

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-l") 'smart-open-line)

(set-window-margins nil 30)

(global-auto-revert-mode t)
(diminish 'auto-revert-mode)
(diminish 'auto-fill-mode)
(diminish 'enrich-mode)



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


(defun scala-test-get-test-name-at-line ()
  (progn
    (string-match "^ *it.*?\"\\(.*\\)\"" (thing-at-point 'line t))
    (match-string 1 (thing-at-point 'line t))))

(defun ensime-sbt-do-run-single-test ()
  (interactive)
  (sbt-command (format "testOnly %s -- -z \"%s\"" (ensime-top-level-class-closest-to-point) (scala-test-get-test-name-at-line))))

(defun ensime-sbt-do-run-suite ()
  (interactive)
  (sbt-command (format "testOnly %s" (ensime-top-level-class-closest-to-point))))


(defun ensime-sbt-do-run-testQuick-repeated ()
  (interactive)
  (sbt-command "testQuick"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("5d5bc275d76f782fcb4ca2f3394031f4a491820fd648aed5c51efc15d472562e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "613a7c50dbea57860eae686d580f83867582ffdadd63f0f3ebe6a85455ab7706" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "6c62b1cd715d26eb5aa53843ed9a54fc2b0d7c5e0f5118d4efafa13d7715c56e" default)))
 '(fci-rule-color "#373b41")
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
 '(powerline-height 20)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-expand-btn-face ((t (:background "color-236" :foreground "#5fafd7")))))


(load-theme 'spacemacs-dark)


;; ------------------------------

(defun buffer-dir-name ()
  (-when-let (file buffer-file-name)
    (file-name-directory file)))

(defun update-neotree ()
  (interactive)
  (let ((current (current-buffer)))
    (-when-let (file (buffer-file-name))
      (neo-global--open-and-find file))
    (hl-line-highlight)
    (switch-to-buffer current)))
(put 'downcase-region 'disabled nil)
