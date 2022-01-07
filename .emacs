;;;; This is my .emacs. There are many like it, but this one is mine.

(setq shell-file-name "/bin/sh")

(require 'package)

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package vs-light-theme
  :config
  (load-theme 'vs-light t))

(use-package emacs
  :init
  (unbind-key "C-x l" global-map)
  (setq use-package-always-defer t
	backup-directory-alist `((".*" . ,temporary-file-directory))
	auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	frame-title-format '((:eval (if (projectile-project-p)
					(concat "[" (projectile-project-name) "]/"
						(file-relative-name buffer-file-name
								    (projectile-project-root)))
				      (buffer-name (current-buffer))))
			     (vc-mode vc-mode) " (%m)"))
  (when (daemonp)
    (exec-path-from-shell-initialize))
  :diminish visual-line-mode hi-lock-mode)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package company
  :diminish company-mode)

(use-package tramp
  :init
  (setq tramp-backup-directory-alist backup-directory-alist))

(use-package pretty-mode)

(use-package pixel-scroll-precision-mode)

;;;; APPS

(defun mu4e-goodies-detach-view-to-window ()
  "Detach the current mu4e-view buffer from header to a new window."
  (interactive)
  (when (string= (buffer-name (current-buffer)) mu4e~view-buffer-name)
    (rename-buffer (mu4e-msg-field mu4e~view-msg :subject) t)
    (setq mu4e~view-buffer nil)
    (split-window-below)
    (mu4e-view mu4e~view-msg)))

(use-package mu4e
  :commands
  (mu4e)
  :init
  (defun mu4e~draft-open-file (path)
    "Open the the draft file at PATH."
    (if mu4e-compose-in-new-frame
	(find-file-other-frame path)
      (find-file path))
    (mime-to-mml))
  :bind (:map mu4e-view-mode-map ("'" . mu4e-goodies-detach-view-to-window))
  :config
  (use-package mu4e-maildirs-extension)
  (use-package mu4e-conversation)
  (mu4e-maildirs-extension))

(use-package nov
  :config
  (push '("\\.epub\\'" . nov-mode) auto-mode-alist))

(use-package smudge
  :config
  (setq smudge-transport 'connect))

(use-package telega
  :config
  (setq telega-use-images t))

;;;; PROJECT MANAGEMENT

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package flycheck-haskell)

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package helm
  :diminish helm-mode
  :bind (("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
	 ("M-x" . helm-M-x))
  :config
  (use-package helm-ghc)
  (use-package helm-projectile)
  (helm-projectile-on))

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package projectile
  :bind (("C-x p" . projectile-commander)))

(use-package restclient)

(use-package which-key
  :diminish which-key-mode)

(use-package direnv)

;;;; PROGRAMMING LANGUAGES

(use-package nix-mode
  :mode "\\.nix\\'")

(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))

;(load "ProofGeneral/generic/proof-site")

(use-package haskell-mode)

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package sql)

(use-package sql-clickhouse)

(use-package protobuf-mode)

(use-package toml-mode)

(use-package rustic)

(use-package cargo)

;;;; LANGUAGE SERVER PROTOCOL

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-x l")
  :hook
  (haskell-mode . lsp-deferred)
  (haskell-literate-mode . lsp-deferred)
  (scala-mode . lsp)
  (java-mode . lsp)
  (python-mode . lsp)
  (tex-mode . lsp)
  (latex-mode . lsp)
  (sh-mode . lsp)
  (typescript-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  (lsp lsp-deferred)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  (setq lsp-prefer-flymake nil))

(use-package lsp-haskell)

(use-package lsp-metals)

(use-package lsp-typescript)

(use-package lsp-ui)

(use-package lsp-treemacs
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(use-package posframe)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package company-lsp)

;;;;;;HERE GO CUSTOM SET VARIABLES;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 80)
 '(TeX-PDF-mode t)
 '(TeX-command-extra-options "-shell-escape")
 '(TeX-engine 'luatex)
 '(TeX-parse-self t)
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(agda2-fontset-name nil)
 '(agda2-highlight-level 'interactive)
 '(auto-save-default nil)
 '(base16-highlight-mode-line 'contrast)
 '(blink-cursor-mode nil)
 '(c-default-style
   '((c-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(cargo-mode-command-build "build --release --features static")
 '(cargo-path-to-bin "cargo")
 '(column-number-mode t)
 '(custom-file nil)
 '(default-input-method "russian-computer")
 '(direnv-always-show-summary nil)
 '(direnv-mode t)
 '(electric-pair-mode t)
 '(epg-gpg-program "gpg2")
 '(fill-column 80)
 '(flycheck-haskell-stack-ghc-executable "stack --no-nix --system-ghc")
 '(font-use-system-font t)
 '(fringe-mode '(nil . 0) nil (fringe))
 '(gdb-many-windows t)
 '(global-flycheck-mode t)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(haskell-doc-show-global-types t)
 '(haskell-font-lock-symbols nil)
 '(haskell-indent-thenelse 1)
 '(haskell-literate-default 'bird)
 '(haskell-mode-hook
   '(haskell-decl-scan-mode haskell-indentation-mode imenu-add-menubar-index interactive-haskell-mode
			    (lambda nil
			      (ghc-Init))))
 '(haskell-stylish-on-save t)
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*GNU Emacs" "\\*Messages" "\\*Completions" "\\*Quail Completions" "\\*fsm-debug" "\\*Help" "\\*Apropos"))
 '(helm-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(lsp-file-watch-threshold 10000)
 '(lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 '(lsp-headerline-breadcrumb-segments '(symbols))
 '(lsp-metals-show-implicit-conversions-and-classes t)
 '(lsp-metals-show-inferred-type t)
 '(lsp-treemacs-sync-mode t)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-sideline-show-hover nil)
 '(lsp-ui-sideline-show-symbol nil)
 '(magit-define-global-key-bindings t)
 '(magit-diff-refine-hunk 'all)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-auto-save-directory nil)
 '(message-citation-line-format "%A %d %B %Y, à %H:%M, %N a écrit:
")
 '(message-citation-line-function 'message-insert-formatted-citation-line)
 '(message-directory "~/.emacs.d/message/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function 'message-send-mail-with-sendmail)
 '(mm-coding-system-priorities '(utf8))
 '(mm-text-html-renderer 'shr)
 '(mu4e-attachment-dir "/home/viv/Downloads")
 '(mu4e-bookmarks
   '(("flag:unread AND NOT flag:list AND NOT flag:trashed AND NOT maildir:/Archive AND NOT maildir:/Spam" "Unread new messages" 117)
     ("flag:unread AND flag:list AND NOT flag:trashed AND NOT maildir:/Archive AND NOT maildir:/Spam" "Unread mailing lists" 108)
     ("date:today..now AND NOT maildir:/Spam" "Today's messages" 116)
     ("date:7d..now AND NOT maildir:/Spam" "Last 7 days" 119)
     ("mime:image/* AND NOT maildir:/Spam" "Messages with images" 112)
     ("flag:attach AND NOT maildir:/Spam" "Messages with attachments" 97)))
 '(mu4e-change-filenames-when-moving t)
 '(mu4e-compose-complete-only-personal t)
 '(mu4e-compose-dont-reply-to-self t)
 '(mu4e-compose-in-new-frame t)
 '(mu4e-compose-signature nil)
 '(mu4e-confirm-quit nil)
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "mbsync -a")
 '(mu4e-maildir "/home/viv/Mail/fastmail")
 '(mu4e-maildirs-extension-use-bookmarks t)
 '(mu4e-refile-folder "/Archive")
 '(mu4e-sent-folder "/Sent")
 '(mu4e-trash-folder "/Trash")
 '(mu4e-user-mail-address-list '("nf@mkmks.org"))
 '(mu4e-view-show-images t)
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(org-agenda-files '("~/Documents/notes"))
 '(org-capture-templates
   '(("n" "Something I thought or heard" entry
      (file "")
      "" :prepend t)))
 '(org-default-notes-file "~/Documents/notes/inbox.org")
 '(org-directory "~/Documents/notes")
 '(org-reverse-note-order t)
 '(projectile-completion-system 'helm)
 '(projectile-globally-ignored-modes
   '("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode" "gnus-.*-mode" "occur-mode" "rcirc-mode"))
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
 '(recentf-mode t)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(size-indication-mode t)
 '(sql-clickhouse-login-params
   '((user :default "default")
     password
     (server :default "localhost")
     port))
 '(sql-connection-alist
   '(("rafal"
      (sql-user "default")
      (sql-password "thisIsADevPassword")
      (sql-server "localhost")
      (sql-port 9091))))
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(tramp-syntax 'default nil (tramp))
 '(url-queue-timeout 30)
 '(user-mail-address "nf@mkmks.org")
 '(vc-follow-symlinks t)
 '(vhdl-upper-case-attributes t)
 '(vhdl-upper-case-constants nil)
 '(vhdl-upper-case-keywords t)
 '(vhdl-upper-case-types t)
 '(visual-line-fringe-indicators '(left-curly-arrow nil))
 '(visual-line-mode nil t)
 '(vterm-buffer-name-string "%s")
 '(vterm-shell "fish")
 '(which-function-mode nil)
 '(which-key-mode t)
 '(windmove-wrap-around t)
 '(woman-fill-frame t)
 '(woman-use-own-frame nil)
 '(word-wrap t)
 '(xterm-mouse-mode t))

(provide 'emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight semi-bold :height 88 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(lsp-ui-sideline-symbol ((t (:background "blue" :foreground "grey" :box (:line-width -1 :color "grey") :height 0.99)))))
