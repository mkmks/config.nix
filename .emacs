;;;; This is my .emacs. There are many like it, but this one is mine.

(add-to-list 'exec-path (concat (expand-file-name "~/") ".cabal/bin"))
(add-to-list 'exec-path (concat (expand-file-name "~/") ".nix-profile/bin"))
(setenv "PATH" (concat "/var/setuid-wrappers:"
		       (expand-file-name "~/") ".cabal/bin:"
		       (expand-file-name "~/") ".nix-profile/bin:"
		       (getenv "PATH")))
(setenv "TERM" "screen-256color")
;; (setenv "GPG_AGENT_INFO" (concat (getenv "XDG_RUNTIME_DIR")
;; 				 "/gnupg/S.gpg-agent"))
(setenv "SSH_AUTH_SOCK"  (concat (getenv "XDG_RUNTIME_DIR")
				 "/gnupg/S.gpg-agent.ssh"))

(setenv "NIX_GHC" "/run/current-system/sw/bin/ghc")
(setenv "NIX_GHCPKG" "/run/current-system/sw/bin/ghc-pkg")
(setenv "NIX_GHC_LIBDIR" "/run/current-system/sw/lib/ghc-8.0.2/")
(setenv "NIX_GHC_DOCDIR" "/run/current-system/sw/share/x86_64-linux-ghc-8.0.2/")
(setq shell-file-name "/bin/sh")

;;; loading packages

(add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp")
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")

;(global-set-key (kbd "C-x C-f") 'set-fill-column)
;(global-set-key (kbd "C-x s") 'save-buffer)
;(global-set-key (kbd "C-x C-s") 'save-some-buffers)

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

(use-package boon-colemak
  :diminish boon-local-mode
  :disabled)

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

(use-package mu4e
  :init  
  :bind (("C-x m" . mu4e))
  :config
  (use-package mu4e-maildirs-extension)
  (use-package mu4e-conversation)
  (mu4e-maildirs-extension))

;; instant messaging
(require 'erc-services)
;(erc :server "localhost" :port "6667" :nick "mkmks")

;; development
(require 'pretty-mode)
(require 'nix-mode)
;(require 'llvm-mode)

(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))

;(load "ProofGeneral/generic/proof-site")

(require 'haskell-interactive-mode)
(require 'haskell-process)

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;(autoload 'ghc-init "ghc" nil t)
;(autoload 'ghc-debug "ghc" nil t)
;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; (eval-after-load "haskell-interactive-mode"
;;   '(progn
;;      (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
;;      (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)))

;; (eval-after-load "haskell-mode"
;;        '(progn
;; 	  (define-key haskell-mode-map (kbd "C-x C-d") nil)
;; 	  (define-key haskell-mode-map (kbd "C-c C-s") 'ghc-case-split)
;; 	  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;; 	  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;; 	  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
;; 	  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;; 	  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;; 	  (define-key haskell-mode-map (kbd "C-c M-.") nil)
;; 	  (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;;;; just before we are ready


  
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save backups of tramp edits in the same place as other backups
(require 'tramp)
(setq tramp-backup-directory-alist backup-directory-alist)

; replace mode lines with frame titles
(setq frame-title-format
      '((:eval (if (projectile-project-p)
		   (concat "[" (projectile-project-name) "]/"
			   (file-relative-name buffer-file-name (projectile-project-root)))
		 buffer-file-name))
	(vc-mode vc-mode) " (%m)"))

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

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
 '(battery-mode-line-format " %b%p%")
 '(boon-special-mode-list
   (quote
    (Buffer-menu-mode debugger-mode ediff-mode git-rebase-mode org-agenda-mode cfw:calendar-mode ereader-mode mingus-playlist-mode mingus-browse-mode)))
 '(browse-url-browser-function (quote browse-url-firefox))
 '(c-default-style
   (quote
    ((c-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")))
 '(custom-file nil)
 '(custom-safe-themes
   (quote
    ("25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "ef1e992ef341e86397b39ee6b41c1368e1b33d45b0848feac6a8e8d5753daa67" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "f6f5d5adce1f9a764855c9730e4c3ef3f90357313c1cae29e7c191ba1026bc15" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "80930c775cef2a97f2305bae6737a1c736079fdcc62a6fdf7b55de669fbbcd13" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" default)))
 '(default-input-method "russian-computer")
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-load-average-threshold 1.0)
 '(display-time-mail-directory "~/Mail/Inbox/new")
 '(display-time-mail-string "✉")
 '(display-time-use-mail-icon t)
 '(electric-pair-mode t)
 '(elscreen-display-screen-number nil)
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(epg-gpg-program "gpg2")
 '(erc-auto-query nil)
 '(erc-enable-logging nil)
 '(erc-nick "mkmks")
 '(erc-nickserv-identify-mode (quote nick-change))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-query-display (quote frame))
 '(erc-server "localhost")
 '(erc-services-mode t)
 '(erc-timestamp-format "[%H:%M:%S]")
 '(erc-timestamp-format-right " [%H:%M:%S]")
 '(erc-track-enable-keybindings nil)
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
 '(ibuffer-never-show-predicates
   (quote
    ("*GNU Emacs*" "*scratch*" "*Messages*" "*Completions*" "*Quail Completions*" "*fsm-debug*" "*Help*" "*Apropos*" "*Mingus")) nil (ibuf-ext))
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Shell"
       (or
	(mode . term-mode)
	(mode . shell-mode)))
      ("Chats"
       (mode . rcirc-mode))
      ("Org"
       (mode . org-mode))
      ("Texts"
       (or
	(filename . ".*.tex$")
	(filename . ".*.md$")))))))
 '(indicate-empty-lines t)
 '(inferior-lisp-program "/usr/bin/clisp")
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-auto-save-directory nil)
 '(message-directory "~/.emacs.d/message/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mm-text-html-renderer (quote shr))
 '(mode-line-format nil)
 '(mu4e-attachment-dir "/home/viv/Downloass")
 '(mu4e-bookmarks
   (quote
    (("flag:unread AND NOT flag:list AND NOT flag:trashed AND NOT maildir:Archive AND NOT maildir:Spam" "Unread new messages" 117)
     ("flag:unread AND flag:list AND NOT flag:trashed AND NOT maildir:Archive AND NOT maildir:Spam" "Unread mailing lists" 108)
     ("date:today..now AND NOT maildir:Spam" "Today's messages" 116)
     ("date:7d..now AND NOT maildir:Spam" "Last 7 days" 119)
     ("mime:image/* AND NOT maildir:Spam" "Messages with images" 112)
     ("flag:attach AND NOT maildir:/Spam" "Messages with attachments" 97))))
 '(mu4e-change-filenames-when-moving t)
 '(mu4e-compose-complete-only-personal t)
 '(mu4e-compose-dont-reply-to-self t)
 '(mu4e-compose-signature nil)
 '(mu4e-confirm-quit nil)
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "mbsync -a")
 '(mu4e-maildir "/home/viv/Mail")
 '(mu4e-maildirs-extension-use-bookmarks t)
 '(mu4e-refile-folder "/Archive")
 '(mu4e-sent-folder "/Sent")
 '(mu4e-trash-folder "/Trash")
 '(mu4e-user-mail-address-list
   (quote
    ("nf@mkmks.org" "frolov@chalmers.se" "comments@mixailkain.net")))
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
    (mu4e-conversation mu4e-maildirs-extension pdf-tools csv-mode base16-theme hide-mode-line nix-mode delight avy evil fancy-battery spaceline boon powerline term-projectile smooth-scrolling use-package dante company slack ereader markdown-mode pass pretty-mode matlab-mode magit log4e llvm-mode ht helm-projectile helm-ghc helm-ag auctex ag)))
 '(projectile-completion-system (quote helm))
 '(projectile-global-mode t)
 '(projectile-globally-ignored-modes
   (quote
    ("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode" "gnus-.*-mode" "occur-mode" "rcirc-mode")))
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
 '(rcirc-default-nick "mkmks")
 '(rcirc-log-flag nil)
 '(rcirc-server-alist (quote (("localhost"))))
 '(rcirc-time-format "%H:%M:%S")
 '(recentf-mode t)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(size-indication-mode t)
 '(smtpmail-default-smtp-server "smtp.fastmail.com")
 '(smtpmail-smtp-server "smtp.fastmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-smtp-user "nf@mkmks.org")
 '(smtpmail-stream-type (quote ssl))
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-c C-x" . execute-extended-command)
     ("C-c C-j" . term-line-mode))))
 '(term-suppress-hard-newline t)
 '(term-unbind-key-list (quote ("C-z" "C-x" "C-c")))
 '(terminal-scrolling nil)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(tramp-syntax (quote url) nil (tramp))
 '(url-queue-timeout 30)
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
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#e0e0e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :family "DejaVu Sans Mono" :foundry "PfEd")))))
