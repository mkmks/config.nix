;;;; This is my .emacs. There are many like it, but this one is mine.

(setq exec-path '("/Users/viv/.cabal/bin" "/Users/viv/.nix-profile/bin" "/nix/store/4znm54fqrrk6nvxxnll8m2a37y4vp0pp-emacs-24.5/libexec/emacs/24.5/x86_64-apple-darwin15.2.0"))

(if (eq system-type 'darwin)
    (let (osx-paths)
      (dolist (path '("/Users/viv/.nix-profile/bin" "/Users/viv/.cabal/bin")
;		    (setenv "PATH" (concat osx-paths (getenv "PATH"))))
		    (setenv "PATH" (concat osx-paths ())))
	;(push path exec-path)
	(setq osx-paths (concat (concat path ":") osx-paths)))))

(cd (expand-file-name "~/"))

(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")

;;; loading packages

(require 'cl)
(require 'package)
(package-initialize)

(require 'pretty-mode)
(require 'tramp)
;(require 'llvm-mode)

(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))

(setq agda2-include-dirs
      (cons "." (mapcar 'expand-file-name
			'("~/agda-stdlib/src" "~/ornaments"))))


(load "ProofGeneral/generic/proof-site")

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

;; keybindings

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-g") 'helm-ag)
(global-unset-key (kbd "s-q"))

; workaround for Terminal.app
(define-key key-translation-map [(control c) (.)] [(control c) (control .)])
(define-key key-translation-map [(control c) (\,)] [(control c) (control \,)])
(define-key key-translation-map [(control c) (=)] [(control c) (control =)])
(define-key key-translation-map [(control c) (\?)] [(control c) (control \?)])

;; minor

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save backups of tramp edits in the same place as other backups
(setq tramp-backup-directory-alist backup-directory-alist)

;; just before we are ready

(unicode-fonts-setup)

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
 '(TeX-view-program-list (quote (("open" "open %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "open")
     (output-html "xdg-open"))))
 '(agda2-fontset-name nil)
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-default-macosx-browser))
 '(browse-url-generic-program "chromium-browser")
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
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" default)))
 '(default-input-method "russian-computer")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average 1)
 '(display-time-format "")
 '(display-time-load-average-threshold 1.0)
 '(display-time-use-mail-icon t)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(haskell-doc-show-global-types t)
 '(haskell-font-lock-symbols nil)
 '(haskell-indent-thenelse 1)
 '(haskell-literate-default (quote bird))
 '(helm-mode t)
 '(ibuffer-mode-hook
   (quote
    ((lambda nil "Always bring up a pretty buffer list."
       (ibuffer-switch-to-saved-filter-groups "default")))))
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
      ("IM"
       (or
	(mode . jabber-roster-mode)
	(mode . jabber-chat-mode)
	(mode . erc-mode)
	(mode . rcirc-mode)))
      ("Mail"
       (or
	(mode . message-mode)
	(mode . notmuch-show-mode)
	(mode . notmuch-search-mode)
	(mode . notmuch-hello-mode)))
      ("Org"
       (mode . org-mode))
      ("Texts"
       (or
	(filename . ".*.tex$")
	(filename . ".*.md$")))
      ("Project-Bau"
       (filename . "src/flexsoc/flexTools/flexCompLLVM/"))
      ("Project-llvm-hs"
       (filename . "src/llvm-hs/"))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(indicate-empty-lines t)
 '(inferior-lisp-program "/usr/bin/clisp")
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(message-auto-save-directory "~/.emacs.d/message/drafts/")
 '(message-directory "~/.emacs.d/message/")
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mm-text-html-renderer nil)
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(projectile-global-mode t)
 '(rcirc-default-nick "mkmks")
 '(rcirc-log-flag nil)
 '(rcirc-server-alist
   (quote
    (("localhost" :channels
      ("#twitter_mkmks"))
     ("irc.freenode.net" :channels
      ("#agda" "#haskell" "##hott")))))
 '(rcirc-time-format "%H:%M:%S")
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(size-indication-mode t)
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
 '(default ((t (:inherit nil :stipple nil :background "gray84" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Menlo"))))
 '(show-paren-match ((t (:background "moccasin")))))
