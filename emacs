;;;; This is my .emacs. There are many like it, but this one is mine.

;;; loading packages

(unless (< emacs-major-version 24)
  (require 'cl)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (defvar my-packages '(melpa zenburn-theme pretty-mode
  auctex haskell-mode scala-mode writegood-mode linum-relative)
  "A list of packages to ensure are installed at launch.")

  (defun install-my-packages ()
    (unless (every 'package-installed-p my-packages)
      (package-refresh-contents)
      (mapcar 'package-install (remove-if 'package-installed-p my-packages))))

  (install-my-packages)

  (require 'pretty-mode))

(require 'tramp)

(unless (not (file-directory-p "~/src/llvm"))
  (add-to-list 'load-path "~/src/llvm/utils/emacs")
  (require 'llvm-mode))

(unless (not (file-directory-p "~/src/gf/"))
	     (load-file "~/src/gf/src/tools/gf.el")
	     (autoload 'run-gf "gf" nil t)
	     (autoload 'gf-mode "gf" nil t)
	     (add-to-list 'auto-mode-alist '("\\.gf\\(\\|e\\|r\\|cm?\\)\\'" . gf-mode))
	     (add-to-list 'auto-mode-alist '("\\.cf\\'" . gf-mode))
	     (add-to-list 'auto-mode-alist '("\\.ebnf\\'" . gf-mode)))

(unless (not (file-directory-p "~/.cabal/share/Agda-2.3.2.1/emacs-mode/"))
  (add-to-list 'load-path "~/.cabal/share/Agda-2.3.2.1/emacs-mode/")
  (require 'agda2))

(unless (not (file-directory-p "/usr/local/Cellar/coq/8.4pl1/lib/emacs/site-lisp/"))
  (add-to-list 'load-path "/usr/local/Cellar/coq/8.4pl1/lib/emacs/site-lisp/")
  (setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
  (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t))

;; keybindings

(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(if (eq system-type 'darwin)
    (let (osx-paths)
      (dolist (path '("/usr/local/bin" "/Users/viv/.cabal/bin" "/usr/texbin")
		    (setenv "PATH" (concat osx-paths (getenv "PATH"))))
	(push path exec-path)
	(setq osx-paths (concat (concat path ":") osx-paths)))))

(unicode-fonts-setup)

(server-start)

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
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "open") (output-html "xdg-open"))))
 '(agda2-fontset-name nil)
 '(agda2-include-dirs (quote ("/Users/viv/src/agda-lib-0.7/src" ".")))
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-default-macosx-browser))
 '(browse-url-generic-program "chromium-browser")
 '(c-default-style (quote ((c-mode . "k&r") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(column-number-mode t)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")))
 '(custom-file nil)
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" default)))
 '(default-input-method "russian-computer")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average 1)
 '(display-time-format "")
 '(display-time-load-average-threshold 1.0)
 '(display-time-mode t)
 '(display-time-use-mail-icon t)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-semantic-idle-summary-mode t)
 '(global-semantic-idle-tag-highlight-mode t nil (semantic/idle))
 '(global-semantic-show-unmatched-syntax-mode t nil (semantic/util-modes))
 '(global-semantic-stickyfunc-mode t nil (semantic/util-modes))
 '(haskell-doc-show-global-types t)
 '(haskell-font-lock-symbols nil)
 '(haskell-indent-thenelse 1)
 '(haskell-literate-default (quote bird))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-haskell-doc-mode turn-on-haskell-decl-scan)))
 '(ibuffer-mode-hook (quote ((lambda nil "Always bring up a pretty buffer list." (ibuffer-switch-to-saved-filter-groups "default")))))
 '(ibuffer-never-show-predicates (quote ("*GNU Emacs*" "*scratch*" "*Messages*" "*Completions*" "*Quail Completions*" "*fsm-debug*" "*Notmuch errors*" "*Help*" "*Apropos*" "*-jabber-roster-*" "*Mingus")) nil (ibuf-ext))
 '(ibuffer-saved-filter-groups (quote (("default" ("Shell" (or (mode . term-mode) (mode . shell-mode))) ("IM" (or (mode . jabber-roster-mode) (mode . jabber-chat-mode) (mode . erc-mode) (mode . rcirc-mode))) ("Mail" (or (mode . message-mode) (mode . notmuch-show-mode) (mode . notmuch-search-mode) (mode . notmuch-hello-mode))) ("Org" (mode . org-mode)) ("Texts" (or (filename . ".*.tex$") (filename . ".*.md$"))) ("Project-Bau" (filename . "src/flexsoc/flexTools/flexCompLLVM/")) ("Project-llvm-hs" (filename . "src/llvm-hs/")) ("Project-GF" (filename . "src/gf/")) ("Project-Kontrakcja" (filename . "src/kontrakcja/"))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(icicle-incremental-completion-delay 0)
 '(icicle-show-Completions-initially-flag t)
 '(icomplete-mode t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\` " "*GNU Emacs*" "*Completions*" "*Quail Completions*" "*fsm-debug*" "*Ibuffer*")))
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/.ido.last")
 '(indicate-empty-lines t)
 '(inferior-lisp-program "/usr/bin/clisp")
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(message-auto-save-directory "~/.emacs.d/message/drafts/")
 '(message-directory "~/.emacs.d/message/")
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mm-text-html-renderer (quote w3m))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-logo nil)
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(projectile-global-mode t)
 '(rcirc-default-nick "mkmks")
 '(rcirc-log-flag t)
 '(rcirc-server-alist (quote (("localhost"))))
 '(rcirc-time-format "%H:%M:%S")
 '(semantic-mode t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob) ("C-c C-x" . execute-extended-command) ("C-c C-j" . term-line-mode))))
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
 '(woman-use-own-frame nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
