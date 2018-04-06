;;;; This is my .emacs. There are many like it, but this one is mine.

(add-to-list 'exec-path (concat (expand-file-name "~/") ".cabal/bin"))
(add-to-list 'exec-path (concat (expand-file-name "~/") ".nix-profile/bin"))
(setenv "PATH" (concat "/var/setuid-wrappers:"
		       (expand-file-name "~/") ".cabal/bin:"
		       (expand-file-name "~/") ".nix-profile/bin:"
		       (getenv "PATH")))
;; (setenv "GPG_AGENT_INFO" (concat (getenv "XDG_RUNTIME_DIR")
;; 				 "/gnupg/S.gpg-agent"))
(setenv "SSH_AUTH_SOCK"  (concat (getenv "XDG_RUNTIME_DIR")
				 "/gnupg/S.gpg-agent.ssh"))

(setenv "NIX_GHC" "/run/current-system/sw/bin/ghc")
(setenv "NIX_GHCPKG" "/run/current-system/sw/bin/ghc-pkg")
(setenv "NIX_GHC_LIBDIR" "/run/current-system/sw/lib/ghc-8.0.2/")
(setenv "NIX_GHC_DOCDIR" "/run/current-system/sw/share/x86_64-linux-ghc-8.0.2/")

;;; loading packages

(add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp")
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")

;(global-set-key (kbd "C-x C-f") 'set-fill-column)
;(global-set-key (kbd "C-x s") 'save-buffer)
;(global-set-key (kbd "C-x C-s") 'save-some-buffers)

(require 'package)
(package-initialize)
(require 'use-package)

(use-package emacs
  :diminish visual-line-mode hi-lock-mode)

(use-package powerline)

(use-package spaceline-config
  :after powerline
  :config
  (spaceline-spacemacs-theme))

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

;; instant messaging
(require 'erc-services)
;(erc :server "localhost" :port "6667" :nick "mkmks")

;; development
(require 'pretty-mode)
;(require 'nix-mode)
(require 'llvm-mode)

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

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;;;;;;HERE GO CUSTOM SET VARIABLES;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "xelatex -shell-escape")
 '(Man-width 80)
 '(TeX-PDF-mode t)
 '(TeX-command-extra-options "-shell-escape")
 '(TeX-engine (quote xetex))
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
 '(agda2-program-name "~/.nix-profile/bin/agda")
 '(auto-save-default nil)
 '(battery-mode-line-format " %b%p%")
 '(boon-special-mode-list
   (quote
    (Buffer-menu-mode debugger-mode ediff-mode git-rebase-mode org-agenda-mode cfw:calendar-mode ereader-mode mingus-playlist-mode mingus-browse-mode)))
 '(browse-url-browser-function (quote browse-url-chrome))
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
 '(custom-enabled-themes (quote (anti-zenburn)))
 '(custom-file nil)
 '(custom-safe-themes
   (quote
    ("bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2cf7f9d1d8e4d735ba53facdc3c6f3271086b6906c4165b12e4fd8e3865469a6" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "6af55f6f26c0c6f113427d8ce72dea34aa1972b70e650486e6c725abd18bbe91" "c58382b9c4fff1aa94b8e3f0f81b0212bb554e83f76957bab735f960a4c441b1" "90b7aaddf859ba6b431c252444d29bab98dd687d2f571707ff70efcb1a2e19f6" "404a8e7f198ef3a5babdf122c7905abc61a8cd04eb2a1ce7d6faec5550b02a90" "37def0fac11a4890922af9febc8394e3b6e3c68904a294a2d440b1904e979c7e" "6a925fdf3a7bf2f3901d8fbc4ef64f9b4b4be2c6bed2b0d49d154db0bec91b33" "5d61bf41bfda37fb1db418b7e41672a081247c4ee8fcf3226d00cd69c1af9fe8" "0ad5a61e6ee6d2e7f884c0da7a6f437a4c84547514b509bdffd06757a8fc751f" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" default)))
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
 '(fancy-battery-mode t)
 '(fill-column 80)
 '(font-use-system-font t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gdb-many-windows t)
 '(global-flycheck-mode t)
 '(global-magit-file-mode t)
 '(global-visual-line-mode t)
 '(gnus-directory "~/.emacs.d/news/")
 '(gnus-home-directory "~/.emacs.d/")
 '(gnus-select-method (quote (nntp "news.gmane.org")))
 '(gnus-use-full-window nil)
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
    (nix-mode delight avy evil fancy-battery spaceline boon powerline term-projectile smooth-scrolling use-package dante company slack ereader markdown-mode pass pretty-mode plan9-theme mingus matlab-mode magit log4e llvm-mode linum-relative ht helm-projectile helm-ghc helm-ag auctex anti-zenburn-theme ag)))
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
 '(tramp-default-method "ssh")
 '(tramp-syntax (quote url))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#c0c0c0" :foreground "#232333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(show-paren-match ((t (:background "moccasin"))))
 '(variable-pitch ((t (:height 110 :family "DejaVu Serif Condensed")))))

(provide 'emacs)
;;; .emacs ends here
