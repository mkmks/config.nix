;;;; This is my .emacs. There are many like it, but this one is mine.

(setq shell-file-name "/bin/sh")

(setq
 package-enable-at-startup nil
 package-archives
 '(("melpa-stable" . "http://stable.melpa.org/packages/")
   ("gnu"         . "http://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)
(require 'use-package)

(use-package emacs
  :diminish visual-line-mode hi-lock-mode)

(use-package base16-theme
  :ensure t
  :init
  (setq base16-theme-256-color-source "colors")
  :config
  (load-theme 'base16-bright t))

(use-package helm
  :diminish helm-mode
  :bind (("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf))
  :config
  (use-package helm-ghc))

(use-package projectile
  :bind (("C-x p" . projectile-commander)))

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package pretty-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))

;(load "ProofGeneral/generic/proof-site")

(use-package haskell-mode)

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package tramp
  :init
  (setq tramp-backup-directory-alist backup-directory-alist))

(defun mu4e-goodies-detach-view-to-window ()
  "Detach the current mu4e-view buffer from header to a new window."
  (interactive)
  (when (string= (buffer-name (current-buffer)) mu4e~view-buffer-name)
    (rename-buffer (mu4e-msg-field mu4e~view-msg :subject) t)
    (setq mu4e~view-buffer nil)
    (split-window-below)
    (mu4e-view mu4e~view-msg)))

(use-package mu4e
  :init  
  :bind (:map mu4e-view-mode-map ("'" . mu4e-goodies-detach-view-to-window))
  :config
  (use-package mu4e-maildirs-extension)
  (use-package mu4e-conversation)
  (mu4e-maildirs-extension))

; replace mode lines with frame titles

(setq frame-title-format
      '((:eval (if (projectile-project-p)
		   (concat "[" (projectile-project-name) "]/"
			   (file-relative-name buffer-file-name (projectile-project-root)))
		 (buffer-name (current-buffer))))
	(vc-mode vc-mode) " (%m)"))

;;;;;;HERE GO CUSTOM SET VARIABLES;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 80)
 '(TeX-PDF-mode t)
 '(TeX-command-extra-options "-shell-escape")
 '(TeX-engine (quote luatex))
 '(TeX-parse-self t)
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(agda2-fontset-name nil)
 '(agda2-highlight-level (quote interactive))
 '(auto-save-default nil)
 '(base16-highlight-mode-line (quote contrast))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(c-default-style
   (quote
    ((c-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(custom-file nil)
 '(custom-safe-themes
   (quote
    ("25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "ef1e992ef341e86397b39ee6b41c1368e1b33d45b0848feac6a8e8d5753daa67" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "f6f5d5adce1f9a764855c9730e4c3ef3f90357313c1cae29e7c191ba1026bc15" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "80930c775cef2a97f2305bae6737a1c736079fdcc62a6fdf7b55de669fbbcd13" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" default)))
 '(default-input-method "russian-computer")
 '(electric-pair-mode t)
 '(epg-gpg-program "gpg2")
 '(fill-column 80)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gdb-many-windows t)
 '(global-flycheck-mode t)
 '(global-linum-mode t)
 '(global-magit-file-mode t)
 '(global-visual-line-mode t)
 '(haskell-doc-show-global-types t)
 '(haskell-font-lock-symbols nil)
 '(haskell-indent-thenelse 1)
 '(haskell-literate-default (quote bird))
 '(haskell-mode-hook
   (quote
    (haskell-decl-scan-mode haskell-indentation-mode imenu-add-menubar-index interactive-haskell-mode
			    (lambda nil
			      (ghc-init)))))
 '(haskell-stylish-on-save t)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*GNU Emacs" "\\*Messages" "\\*Completions" "\\*Quail Completions" "\\*fsm-debug" "\\*Help" "\\*Apropos")))
 '(helm-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-auto-save-directory nil)
 '(message-citation-line-format "%A %d %B %Y, à %H:%M, %N a écrit:
")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-directory "~/.emacs.d/message/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(mm-coding-system-priorities (quote (utf8)))
 '(mm-text-html-renderer (quote shr))
 '(mode-line-format nil)
 '(mu4e-attachment-dir "/home/viv/Downloads")
 '(mu4e-bookmarks
   (quote
    (("flag:unread AND NOT flag:list AND NOT flag:trashed AND NOT maildir:/Archive AND NOT maildir:/Spam" "Unread new messages" 117)
     ("flag:unread AND flag:list AND NOT flag:trashed AND NOT maildir:/Archive AND NOT maildir:/Spam" "Unread mailing lists" 108)
     ("date:today..now AND NOT maildir:/Spam" "Today's messages" 116)
     ("date:7d..now AND NOT maildir:/Spam" "Last 7 days" 119)
     ("mime:image/* AND NOT maildir:/Spam" "Messages with images" 112)
     ("flag:attach AND NOT maildir:/Spam" "Messages with attachments" 97))))
 '(mu4e-change-filenames-when-moving t)
 '(mu4e-compose-complete-only-personal t)
 '(mu4e-compose-dont-reply-to-self t)
 '(mu4e-compose-in-new-frame t)
 '(mu4e-compose-signature nil)
 '(mu4e-confirm-quit nil)
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "mbsync -a")
 '(mu4e-maildir "/home/viv/Mail")
 '(mu4e-maildirs-extension-use-bookmarks t)
 '(mu4e-refile-folder "/Archive")
 '(mu4e-sent-folder "/Sent")
 '(mu4e-trash-folder "/Trash")
 '(mu4e-user-mail-address-list (quote ("nf@mkmks.org")))
 '(mu4e-view-show-images t)
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(org-agenda-files (quote ("~/Documents/notes")))
 '(org-capture-templates
   (quote
    (("n" "Something I thought or heard" entry
      (file "")
      "" :prepend t))))
 '(org-default-notes-file "~/Documents/notes/inbox.org")
 '(org-directory "~/Documents/notes")
 '(org-reverse-note-order t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (0xc erc-services fish-mode dockerfile-mode diminish projectile mu4e-conversation mu4e-maildirs-extension pdf-tools csv-mode base16-theme hide-mode-line nix-mode delight avy evil fancy-battery spaceline boon powerline term-projectile smooth-scrolling use-package dante company slack ereader markdown-mode pass pretty-mode matlab-mode magit log4e llvm-mode ht helm-projectile helm-ghc helm-ag auctex ag)))
 '(projectile-completion-system (quote helm))
 '(projectile-global-mode t)
 '(projectile-globally-ignored-modes
   (quote
    ("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode" "gnus-.*-mode" "occur-mode" "rcirc-mode")))
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
 '(recentf-mode t)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(tramp-syntax (quote default) nil (tramp))
 '(url-queue-timeout 30)
 '(use-package-always-ensure t)
 '(user-mail-address "nf@mkmks.org")
 '(vc-follow-symlinks t)
 '(vhdl-upper-case-attributes t)
 '(vhdl-upper-case-constants nil)
 '(vhdl-upper-case-keywords t)
 '(vhdl-upper-case-types t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow nil)))
 '(visual-line-mode nil t)
 '(which-function-mode nil)
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
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#e0e0e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 79 :width normal :family "DejaVu Sans Mono" :foundry "PfEd")))))
