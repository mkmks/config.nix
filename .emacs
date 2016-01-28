;;;; This is my .emacs. There are many like it, but this one is mine.

(add-to-list 'exec-path (concat (expand-file-name "~/") ".nix-profile/bin"))
(setenv "PATH" (concat "/var/setuid-wrappers:"
		       (expand-file-name "~/") ".nix-profile/bin:"
		       (getenv "PATH")))


(add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp")
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")

;;; loading packages

(require 'cl)
(require 'package)
(package-initialize)

(require 'pretty-mode)
(require 'tramp)
(require 'nix-mode)
(require 'llvm-mode)

(require 'sane-term)
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)

(require 'mu4e)
(require 'mu4e-maildirs-extension)

(global-set-key (kbd "C-x m") 'mu4e)
(global-set-key (kbd "C-x w") 'elfeed)

(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message))
      (when msg
	(setq user-mail-address
	  (cond
	    ((mu4e-message-contact-field-matches msg :to "frolov@chalmers.se")
	      "frolov@chalmers.se")
	    (t "nf@mkmks.org")))))))

(mu4e-maildirs-extension)

(add-hook 'rcirc-mode-hook
          (lambda ()
            (load-file "~/.rcirc-authinfo.el.gpg")))

(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)

(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))

(setq agda2-include-dirs
      (cons "." (mapcar 'expand-file-name
			'("~/.nix-profile/share/agda" "~/ornaments"))))

;(load "ProofGeneral/generic/proof-site")

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(add-hook 'haskell-mode-hook 'haskell-simple-indent-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(eval-after-load "haskell-mode"
       '(progn
         (define-key haskell-mode-map (kbd "C-x C-d") nil)
         (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
         (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
         (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
         (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
         (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
         (define-key haskell-mode-map (kbd "C-c M-.") nil)
         (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(require 'helm-ghc)

;; just before we are ready

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-g") 'helm-ag)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save backups of tramp edits in the same place as other backups
(setq tramp-backup-directory-alist backup-directory-alist)

(elscreen-start)
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;HERE GO CUSTOM SET VARIABLES;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(Man-width 80)
 '(TeX-PDF-mode t)
 '(TeX-parse-self t)
 '(agda2-fontset-name nil)
 '(auto-save-default nil)
 '(browse-url-browser-function (quote browse-url-chromium))
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
    ("6a925fdf3a7bf2f3901d8fbc4ef64f9b4b4be2c6bed2b0d49d154db0bec91b33" "5d61bf41bfda37fb1db418b7e41672a081247c4ee8fcf3226d00cd69c1af9fe8" "0ad5a61e6ee6d2e7f884c0da7a6f437a4c84547514b509bdffd06757a8fc751f" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" default)))
 '(default-input-method "russian-computer")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average 1)
 '(display-time-format "")
 '(display-time-load-average-threshold 1.0)
 '(display-time-use-mail-icon t)
 '(electric-pair-mode t)
 '(elfeed-enclosure-default-dir "/home/viv/Downloads")
 '(elfeed-feeds
   (quote
    ("https://pigworker.wordpress.com/feed/" "http://officialandroid.blogspot.com/feeds/posts/default" "http://bartoszmilewski.com/feed/" "http://code.facebook.com/posts/rss" "http://gmailblog.blogspot.com/atom.xml" "http://newsroom.fb.com/feed/" "https://existentialtype.wordpress.com/feed/" "http://googleresearch.blogspot.com/atom.xml" "http://semantic-domain.blogspot.com/feeds/posts/default" "http://googleblog.blogspot.com/atom.xml" "http://math.andrej.com/feed/" "http://research.facebook.com/blog/rss" "http://researchblogs.cs.bham.ac.uk/thelablunch/feed/" "http://feeds.feedburner.com/ezyang" "http://www.jonmsterling.com/rss.xml" "http://sorhed.livejournal.com/data/rss" "http://users.livejournal.com/_devol_/data/rss" "http://bohemicus.livejournal.com/data/rss" "http://salery.livejournal.com/data/rss" "http://akuklev.livejournal.com/data/rss" "http://resfed.com/feed" "http://krylov.livejournal.com/data/rss" "http://asterrot.livejournal.com/data/rss" "http://galkovsky.livejournal.com/data/rss" "http://arbat.livejournal.com/data/rss" "http://tttkkk.livejournal.com/data/rss")))
 '(elscreen-persist-mode t)
 '(epg-gpg-program "gpg2")
 '(fill-column 80)
 '(font-use-system-font t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(haskell-doc-show-global-types t)
 '(haskell-font-lock-symbols nil)
 '(haskell-indent-thenelse 1)
 '(haskell-literate-default (quote bird))
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*GNU Emacs" "\\*Messages" "\\*Completions" "\\*Quail Completions" "\\*fsm-debug" "\\*Help" "\\*Apropos")))
 '(helm-mode t)
 '(ibuffer-never-show-predicates
   (quote
    ("*GNU Emacs*" "*scratch*" "*Messages*" "*Completions*" "*Quail Completions*" "*fsm-debug*" "*Notmuch errors*" "*Help*" "*Apropos*" "*-jabber-roster-*" "*Mingus")) nil (ibuf-ext))
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Shell"
       (or
	(mode . term-mode)
	(mode . shell-mode)))
      ("Mail"
       (or
	(mode . message-mode)
	(mode . mu4e-compose-mode)
	(mode . mu4e-main-mode)
	(mode . mu4e-headers-mode)
	(mode . mu4e-view-mode)))
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
 '(mail-user-agent (quote mu4e-user-agent))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-auto-save-directory nil)
 '(message-directory "~/.emacs.d/message/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mm-text-html-renderer nil)
 '(mu4e-attachment-dir "/home/viv/Downloads")
 '(mu4e-bookmarks
   (quote
    (("flag:unread AND NOT flag:trashed AND NOT flag:list AND NOT maildir:\"/[Gmail]/.All Mail\"" "Unread messages" 117)
     ("flag:unread AND flag:list AND NOT flag:trashed" "Unread mailing lists" 108)
     ("date:today..now" "Today's messages" 116)
     ("date:7d..now" "Last 7 days" 119)
     ("mime:image/*" "Messages with images" 112))))
 '(mu4e-change-filenames-when-moving t)
 '(mu4e-compose-complete-only-personal t)
 '(mu4e-compose-dont-reply-to-self t)
 '(mu4e-compose-signature nil)
 '(mu4e-drafts-folder "/[Gmail]/.Drafts")
 '(mu4e-headers-date-format "%F %R")
 '(mu4e-headers-fields
   (quote
    ((:human-date . 16)
     (:flags . 6)
     (:mailing-list . 10)
     (:from . 22)
     (:subject))))
 '(mu4e-headers-skip-duplicates t)
 '(mu4e-maildir "/home/viv/Mail")
 '(mu4e-maildir-shortcuts
   (quote
    (("\"/Inbox\"" . 105)
     ("/[Gmail]/.Sent Mail" . 115)
     ("/[Gmail]/.Starred" . 43)
     ("/[Gmail]/.Trash" . 116)
     ("/[Gmail]/.All Mail" . 97))))
 '(mu4e-maildirs-extension-use-bookmarks t)
 '(mu4e-sent-folder "/[Gmail]/.Sent Mail")
 '(mu4e-sent-messages-behavior (quote delete))
 '(mu4e-trash-folder "/[Gmail]/.Trash")
 '(mu4e-user-mail-address-list (quote ("nf@mkmks.org" "frolov@chalmers.se")))
 '(mu4e-view-show-images t)
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(projectile-completion-system (quote helm))
 '(projectile-global-mode t)
 '(projectile-globally-ignored-modes
   (quote
    ("erc-mode" "help-mode" "completion-list-mode" "Buffer-menu-mode" "gnus-.*-mode" "occur-mode" "rcirc-mode" "mu4e-.*-mode")))
 '(rcirc-default-nick "mkmks")
 '(rcirc-log-flag nil)
 '(rcirc-server-alist (quote (("localhost"))))
 '(rcirc-time-format "%H:%M:%S")
 '(rcirc-track-minor-mode t)
 '(recentf-mode t)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(size-indication-mode t)
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-stream-type (quote starttls))
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-c C-x" . execute-extended-command)
     ("C-c C-j" . term-line-mode))))
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
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#c0c0c0" :foreground "#232333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "Inconsolata"))))
 '(elfeed-search-feed-face ((t (:foreground "#6c1f1c" :family "DejaVu Sans Mono"))))
 '(elfeed-search-title-face ((t (:foreground "#000" :family "DejaVu Sans Mono"))))
 '(mu4e-header-highlight-face ((t (:inherit region :underline t))))
 '(show-paren-match ((t (:background "moccasin")))))
