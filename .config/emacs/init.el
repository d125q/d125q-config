;;; init.el --- d125q's init file  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210408112146
;; Keywords: convenience

;;; Commentary:

;; When GNU Emacs is started, it normally tries to load a Lisp program
;; from an initialization file, or init file for short.  This file, if
;; it exists, specifies how to initialize GNU Emacs.

;;; Code:

;; * Preamble

(require 'cl-lib)
(require 'd125q-lib)

(defvar d125q-secret-plist)           ; defined in the early init file

(d125q-define-controls show-trailing-whitespace)
(d125q-define-controls indent-tabs-mode)

;; * Minibuffer

(d125q-customizeq
 enable-recursive-minibuffers t
 minibuffer-auto-raise t
 minibuffer-eldef-shorten-default t
 minibuffer-depth-indicate-mode t
 minibuffer-electric-default-mode t
 savehist-mode t)

;; ** Icomplete

(defun d125q-icomplete-fido-updir-or-backward-kill-sexp ()
  "Go up one directory or kill the preceding S-expression."
  (interactive)
  (declare-function icomplete--category "icomplete")
  (if (eq (icomplete--category) 'file)
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-kill-sexp)))

(d125q-customizeq
 icomplete-prospects-height 2
 icomplete-show-matches-on-no-input t)

(with-eval-after-load 'icomplete
  (defvar icomplete-fido-mode-map)
  (d125q-bind-keys (:map icomplete-fido-mode-map)
    "C-l" d125q-icomplete-fido-updir-or-backward-kill-sexp))

;; ** Ido

(d125q-customizeq
 ido-decorations '("{"
                   "}"
                   " | "
                   " | …"
                   "["
                   "]"
                   " [No match]"
                   " [Matched]"
                   " [Not readable]"
                   " [Too big]"
                   " [Confirm]")
 ido-case-fold t
 ido-enable-flex-matching t
 ido-enable-last-directory-history t
 ido-enable-tramp-completion t
 ido-use-virtual-buffers t
 ido-record-commands t
 ido-cr+-auto-update-blacklist t)

(require 'ido)
(ido-everywhere)
(amx-mode)

;; * Magit

(d125q-customizeq
 magit-define-global-key-bindings t
 magit-completing-read-function 'magit-ido-completing-read)

(require 'magit)
(require 'magit-extras)

;; Ido has already been loaded
(d125q-bind-keys (:map ido-common-completion-map)
  "C-x g" ido-enter-magit-status)

;; * Helm

(d125q-customizeq
 helm-display-function #'helm-default-display-buffer
 helm-command-prefix-key nil
 helm-ff-DEL-up-one-level-maybe t
 helm-ff-display-image-native t
 helm-buffer-max-length 40
 helm-buffer-end-truncated-string "…"
 helm-describe-function-function #'helpful-function
 helm-describe-variable-function #'helpful-variable
 helm-grep-ag-command "rg -n -S --hidden --color always --no-heading %s %s %s")

(require 'helm)
(require 'helm-config)
(unless (load "helm-project-loaddefs" 'noerror 'nomessage)
  (require 'helm-project))

(with-eval-after-load 'helm-files
  (defvar helm-ff-default-directory)
  (defvar helm-find-files-actions)
  (defvar helm-find-files-map)

  (defun d125q-helm-ff-magit-status (candidate)
    "Open the Magit status from Helm FF with an action.

If CANDIDATE is a directory, then it will be used as DIRECTORY to
determine the toplevel; otherwise, `helm-ff-default-directory'
will be used for this purpose."
    (if-let* ((directory (if (file-directory-p candidate)
                             (file-name-as-directory candidate)
                           helm-ff-default-directory))
              (toplevel (magit-toplevel directory)))
        (magit-status-setup-buffer toplevel)
      (user-error "Could not the find the toplevel of %s" directory)))

  (defun d125q-helm-ff-run-magit-status ()
    "Open the Magit status from Helm FF with a key binding."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'd125q-helm-ff-magit-status)))
  (put 'd125q-helm-ff-run-magit-status 'helm-only t)

  (cl-pushnew '("Open the Magit status" . d125q-helm-ff-magit-status)
              (cdr (last helm-find-files-actions)))

  (d125q-bind-keys (:map helm-find-files-map)
    "C-x g" d125q-helm-ff-run-magit-status))

(d125q-bind-keys ()
  "C-c c" helm-resume
  "C-c x" helm-M-x
  "C-c f" helm-find-files
  "C-c F" helm-recentf
  "C-c y" helm-show-kill-ring
  "C-c b" helm-buffers-list
  "C-c B" helm-filtered-bookmarks
  "C-c i" helm-imenu
  "C-c I" helm-imenu-in-all-buffers
  "C-c s" helm-semantic
  "C-c r" helm-register
  "C-c m" helm-mini
  "C-c M" helm-multi-files
  "C-c p" helm-project-switch-project)

(d125q-bind-keys ()
  "s-SPC" helm-all-mark-rings
  "s-:" helm-eval-expression-with-eldoc
  "s-/" helm-dabbrev)

(d125q-bind-keys (:prefix "C-c h")
  "a" helm-apropos
  "g" helm-info-gnus
  "i" helm-info-at-point
  "m" helm-man-woman
  "r" helm-info-emacs)

(d125q-bind-keys (:prefix "C-c s")
  "e" helm-etags-select
  "f" helm-find
  "g" helm-do-grep-ag
  "G" helm-grep-do-git-grep
  "l" helm-locate
  "o" helm-occur
  "r" helm-regexp)

;; * Project

(d125q-bind-keys (:map project-prefix-map)
  "m" magit-project-status)

(d125q-bind-keys (:map project-prefix-map)
  "<f5>" helm-project-find-files
  "<f6>" helm-project-list-buffers
  "<f7>" helm-project-grep
  "<f8>" rg-project
  "<f9>" deadgrep)

(d125q-customizeq
 project-switch-commands '((project-find-file "Find file")
                           (project-find-regexp "Find regexp")
                           (project-dired "Dired")
                           (project-vc-dir "VC-Dir")
                           (magit-project-status "Magit")
                           (project-shell "Shell")
                           (project-eshell "Eshell")
                           (helm-project-find-files "Find files (Helm)")
                           (helm-project-list-buffers "List buffers (Helm)")
                           (helm-project-grep "grep (Helm)")
                           (rg-project "ripgrep")
                           (deadgrep "deadgrep")))

;; * Files

;; ** Auto-save files

(defvar d125q-auto-save-directory (file-name-as-directory
                                   (locate-user-emacs-file "auto-save-files"))
  "Default directory for auto-save files.")

(d125q-customizeq
 auto-save-file-name-transforms `((".*" ,d125q-auto-save-directory t))
 auto-save-default t
 auto-save-no-message t
 delete-auto-save-files t)

;; ** Backup files

(defvar d125q-backup-directory (file-name-as-directory
                                (locate-user-emacs-file "backup-files"))
  "Default directory for backup files.")

(d125q-customizeq
 backup-directory-alist `((".*" . ,d125q-backup-directory))
 tramp-backup-directory-alist backup-directory-alist
 make-backup-files t
 vc-make-backup-files t
 version-control t
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2)

;; ** Operations on files

(d125q-customizeq
 delete-by-moving-to-trash t     ; use the system's trash can
 file-precious-flag t            ; protect against I/O errors
 break-hardlink-on-save t        ; allow breaking of hard links
 vc-follow-symlinks t)           ; follow symbolic links to files under VC

;; ** Auto-revert

(d125q-customizeq
 auto-revert-check-vc-info t
 auto-revert-verbose nil
 global-auto-revert-mode t)

;; ** Save-place

(d125q-customizeq
 save-place-limit nil
 save-place-version-control t
 save-place-mode t)

;; ** Recent files

(d125q-customizeq
 recentf-max-saved-items nil
 recentf-mode t)

(require 'recentf)

(defun d125q-recentf-find-file ()
  "Find a recent file."
  (interactive)
  (if-let* ((alist (mapcar (lambda (fn)
                             (cons (abbreviate-file-name fn) fn))
                           recentf-list))
            (keys (mapcar #'car alist))
            (default (car keys)))
      (let* ((key (completing-read "Find recent file: " keys
                                   nil t nil nil default))
             (entry (assoc key alist #'string=)))
        (find-file (cdr entry)))
    (user-error "No recent files to choose from")))

(d125q-bind-keys (:map recentf-mode-map)
  "s-r" recentf-open-files
  "s-R" d125q-recentf-find-file)

;; * Server

;; for some reason GNOME ignores the `raise-frame' in
;; `server-switch-buffer' and instead only displays a "GNU Emacs is
;; ready" notification, so we use `wmctrl' on top of it
(advice-add 'server-switch-buffer :after (lambda (&rest _)
                                           (when server-raise-frame
                                             (d125q-wmctrl-raise-frame))))

;; * Dired

(d125q-customizeq
 dired-always-read-filesystem t
 dired-auto-revert-buffer t
 dired-create-destination-dirs 'ask
 dired-kept-versions 2
 dired-vc-rename-file t)

;; * Helpful

(d125q-bind-keys (:prefix "s-h")
  "c" helpful-callable
  "f" helpful-function
  "p" helpful-at-point
  "i" helpful-command
  "k" helpful-key
  "m" helpful-macro
  "s" helpful-symbol
  "v" helpful-variable)

;; * Editing

;; ** Killing

(d125q-customizeq
 kill-read-only-ok t
 kill-ring-max 1024
 save-interprogram-paste-before-kill t)

(browse-kill-ring-default-keybindings)

(d125q-bind-keys ()
  "s-k" kill-whole-line)

;; ** Isearch

(d125q-customizeq
 search-ring-max 128
 regexp-search-ring-max 128
 isearch-lazy-count t
 isearch-lazy-highlight t)

;; ** `grep' and friends

(d125q-bind-keys (:map search-map :prefix "g")
  "g" grep
  "v" vc-git-grep
  "f" grep-find
  "l" lgrep
  "r" rgrep
  "z" zrgrep)

(d125q-customizeq
 rg-keymap-prefix (kbd "M-s r")
 rg-use-transient-menu t)

(rg-enable-default-bindings)

(d125q-bind-keys (:map search-map)
  "d" deadgrep
  "l" locate)

;; ** Jumping to things

(d125q-customizeq
 avy-background nil)

(avy-setup-default)
(ace-link-setup-default)

(d125q-bind-keys ()
  "s-'" avy-goto-char-timer
  "s-<return>" ace-window)

;; ** Iedit

(d125q-customizeq
 iedit-toggle-key-default (kbd "s-;"))

(require 'iedit)

;; ** Imenu

(d125q-customizeq
 imenu-auto-rescan t
 imenu-use-popup-menu nil)

(d125q-bind-keys ()
  "s-i" imenu)

;; ** `expand-region'

(d125q-bind-keys ()
  "s-=" er/expand-region)

;; * Ibuffer

(d125q-customizeq
 ibuffer-eliding-string "…")

(d125q-bind-keys ()
  "s-b" ibuffer)

;; * `which-key'

(d125q-customizeq
 which-key-allow-imprecise-window-fit nil
 which-key-compute-remap t
 which-key-show-transient-maps t
 which-key-mode t)

;; * Speedbar

(defun d125q-speedbar-frame-set-input-focus ()
  "Select SPEEDBAR-FRAME, raise and, and set input focus."
  (when (and (boundp 'speedbar-frame) speedbar-frame)
    (select-frame-set-input-focus speedbar-frame)))

(d125q-customizeq
 speedbar-frame-parameters '((undecorated . t)
                             (fullscreen . nil)
                             (minibuffer . nil)
                             (width . 40)
                             (unsplittable . t))
 speedbar-after-create-hook (list #'speedbar-frame-reposition-smartly
                                  #'d125q-speedbar-frame-set-input-focus)
 speedbar-vebosity-level 0)

(d125q-bind-keys ()
  "<f9>" speedbar)

;; * The Customization interface

(d125q-customizeq
 custom-file (locate-user-emacs-file "custom.el")
 custom-buffer-done-kill t
 custom-buffer-verbose-help nil
 custom-raised-buttons t
 custom-search-field nil
 custom-unlispify-tag-names nil
 custom-variable-default-form 'lisp)

(d125q-bind-keys (:prefix "s-c")
  "a" customize-apropos
  "f" customize-face
  "4 f" customize-face-other-window
  "F" customize-apropos-faces
  "g" customize-group
  "4 g" customize-group-other-window
  "G" customize-apropos-groups
  "o" customize-option
  "4 o" customize-option-other-window
  "O" customize-apropos-options
  "v" customize-variable
  "4 v" customize-variable-other-window
  "t" customize-themes
  "r" customize-rogue
  "s" customize-saved
  "u" customize-unsaved
  "c" customize-customized
  "n" customize-changed)

;; * Comint

(d125q-customizeq
 comint-prompt-read-only t)

(d125q-bind-keys (:map comint-mode-map)
  [remap kill-whole-line] comint-kill-whole-line
  [remap kill-region] comint-kill-region)

;; * Mail and News

(d125q-customizeq
 gnus-completing-read-function 'gnus-ido-completing-read
 gnus-dired-mail-mode 'gnus-user-agent
 gnus-picon-style 'right
 compose-mail-user-agent-warnings nil
 mail-user-agent 'gnus-user-agent
 mail-use-rfc822 t
 mm-discouraged-alternatives '("text/html" "text/richtext")
 mm-text-html-renderer 'gnus-w3m)

(d125q-plist-customizeq (d125q-secret-plist :mail)
  send-mail-function
  message-send-mail-function
  smtpmail-default-smtp-server
  smtpmail-smtp-service
  smtpmail-smtp-user)

(d125q-plist-customizeq (d125q-secret-plist :gnus)
  gnus-select-method
  gnus-secondary-select-methods)

;; * IRC

(d125q-plist-customizeq (d125q-secret-plist :erc)
  erc-nick
  erc-server
  erc-port
  erc-user-full-name
  erc-nickserv-passwords)

(with-eval-after-load 'erc
  (defvar erc-password)
  (d125q-plist-setq (d125q-secret-plist :erc)
    erc-password))

(d125q-customizeq
 erc-interpret-mirc-color t
 erc-modules '(autojoin
               button
               completion
               fill
               irccontrols
               list
               match
               move-to-prompt
               netsplit
               networks
               noncommands
               notifications
               readonly
               ring
               services
               spelling
               stamp
               track)
 erc-prompt-for-password nil
 erc-prompt-for-nickserv-password nil
 erc-warn-about-blank-lines t)

;; * Editing source code

(d125q-customizeq
 prog-mode-hook (list #'turn-on-show-trailing-whitespace
                      #'display-line-numbers-mode
                      #'flyspell-prog-mode)
 company-show-numbers t
 company-tooltip-align-annotations t
 company-tooltip-flip-when-above t
 global-company-mode t)

(with-eval-after-load 'flymake
  (defvar flymake-mode-map)
  (d125q-define-transient-map (flymake "C-c !" :map flymake-mode-map)
    ("n" flymake-goto-next-error :persist t)
    ("p" flymake-goto-prev-error :persist t)
    ("s" flymake-start)
    ("l" flymake-switch-to-log-buffer)
    ("d" flymake-show-diagnostics-buffer)
    ("r" flymake-running-backends)
    ("D" flymake-disabled-backends)
    ("R" flymake-reporting-backends)))

;; ** Emacs Lisp

(defun d125q-emacs-lisp-outline-level ()
  "Compute the outline level for Emacs Lisp code."
  (defvar outline-heading-alist)
  (or (cdr (assoc (match-string 0) outline-heading-alist))
      (- (match-end 1) (match-beginning 1))))

(defun d125q-emacs-lisp-setup-outline ()
  "Setup outlines for Emacs Lisp code."
  (declare-function outline-hide-sublevels "outline")
  (setq-local outline-regexp ";; \\(\\*+\\) .*$"
              outline-level #'d125q-emacs-lisp-outline-level)
  (outline-minor-mode)
  (outline-hide-sublevels 1))

(d125q-customizeq
 flycheck-emacs-lisp-load-path 'inherit
 flycheck-emacs-lisp-initialize-packages t)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'d125q-emacs-lisp-setup-outline)

;; ** Python

(d125q-customizeq
 python-indent-guess-indent-offset-verbose nil
 python-shell-prompt-detect-failure-warning nil)

(d125q-bind-keys (:prefix "s-v")
  "a" pyvenv-activate
  "w" pyvenv-workon
  "d" pyvenv-deactivate
  "c" pyvenv-create)

(with-eval-after-load 'pyvenv
  (pyvenv-mode)
  (pyvenv-tracking-mode))

(add-hook 'python-mode-hook #'eglot-ensure)

;; ** R and friends (via ESS)

(d125q-customizeq
 ess-use-R-completion t
 ess-use-auto-complete nil
 ess-use-company t
 ess-R-argument-suffix "="
 ess-use-eldoc t
 ess-eldoc-show-on-symbol t
 ess-use-ido t
 ess-ido-flex-matching t
 ess-use-flymake t
 ess-imenu-use-p t
 ess-use-toolbar nil
 ess-use-tracebug t
 ess-history-directory (file-name-as-directory
                        (locate-user-emacs-file "ess-history"))
 ess-use-inferior-program-in-buffer-name t
 ess-plain-first-buffername nil)

;; ** SQL

(cl-pushnew (cons "\\.\\(?:fnc\\|pck\\|prc\\)\\'" 'sql-mode) auto-mode-alist)

(defvar d125q-sql-product-alist '(("Exasol" . ansi)
                                  ("Oracle" . oracle))
  "Alist mapping project root to SQL product.")

(defun d125q-set-sql-product ()
  "Set `sql-product' according to the current project."
  (declare-function project-root "project")
  (when-let* ((project (project-current))
              (project-root (project-root project))
              (key (file-name-nondirectory (directory-file-name project-root)))
              (product (cdr (assoc key d125q-sql-product-alist #'equal))))
    (message "Setting `sql-product' to `%s' in %s" product project-root)
    (setq-local sql-product product)))

(d125q-customizeq
 sql-product 'ansi
 sql-mode-hook (list #'turn-off-indent-tabs-mode
                     #'turn-off-show-trailing-whitespace
                     #'sql-indent-enable
                     #'d125q-set-sql-product))

(d125q-plist-customizeq (d125q-secret-plist :sql)
  sql-connection-alist)

;; ** Shell scripts

(defun d125q-sh-outline-level ()
  "Compute the outline level for Shell scripts."
  (defvar outline-heading-alist)
  (or (cdr (assoc (match-string 0) outline-heading-alist))
      (- (match-end 1) (match-beginning 1))))

(defun d125q-sh-setup-outline ()
  "Setup outlines for Shell scripts."
  (declare-function outline-hide-sublevels "outline")
  (setq-local outline-regexp "## \\(\\*+\\).*$"
              outline-level #'d125q-sh-outline-level)
  (outline-minor-mode)
  (outline-hide-sublevels 1))

(add-hook 'sh-mode-hook #'d125q-sh-setup-outline)

;; * Editing human-readable text

(d125q-customizeq
 text-mode-hook (list #'turn-on-flyspell
                      #'turn-on-auto-fill
                      #'display-line-numbers-mode
                      #'visual-line-mode
                      #'text-mode-hook-identify))

(d125q-customizeq
 flyspell-issue-message-flag nil
 flyspell-issue-welcome-flag nil
 flyspell-use-meta-tab nil
 ispell-highlight-face 'flyspell-incorrect)

;; ** TeX and friends

;; *** TeX

(d125q-customizeq
 TeX-Omega-command "aleph"
 TeX-parse-self t
 TeX-parse-all-errors t
 TeX-auto-save t
 TeX-byte-compile t
 TeX-clean-confirm nil
 TeX-debug-bad-boxes t
 TeX-debug-warnings t
 TeX-ignore-warnings nil
 TeX-display-help 'expert
 TeX-electric-escape t
 TeX-electric-math (cons "\\(" "\\)")
 TeX-electric-sub-and-superscript t
 TeX-insert-braces t
 TeX-source-correlate-start-server t
 TeX-mode-hook (list #'TeX-PDF-mode
                     #'TeX-fold-mode
                     #'TeX-source-correlate-mode
                     #'flymake-mode)
 TeX-shell (executable-find "zsh")
 TeX-raise-frame-function #'wmctrl-raise-frame
 TeX-view-program-list '(("Zathura (Fullscreen)"
                          ("zathura --mode fullscreen %o"
                           (mode-io-correlate " --synctex-forward %n:0:\"%b\""))
                          "zathura"))
 TeX-view-program-selection '(((output-dvi has-no-display-manager) "dvi2tty")
                              ((output-dvi style-pstricks) "dvips and gv")
                              (output-dvi "xdvi")
                              (output-pdf "Zathura (Fullscreen)")
                              (output-html "xdg-open")))

(autoload 'TeX-revert-document-buffer "tex-buf")
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; *** LaTeX

(d125q-customizeq
 LaTeX-Omega-command "lamed"
 LaTeX-default-style "scrartcl"
 LaTeX-default-options '("version=last" "paper=A4")
 LaTeX-electric-left-right-brace nil
 LaTeX-enable-toolbar nil
 LaTeX-mode-hook (list #'LaTeX-math-mode
                       #'reftex-mode))

;; *** BibTeX

(d125q-customizeq
 bibtex-dialect 'biblatex
 bibtex-entry-format t
 bibtex-field-kill-ring-max 128
 bibtex-maintain-sorted-entries 'entry-class
 bibtex-parse-keys-fast nil)

;; *** RefTeX

(d125q-customizeq
 reftex-plug-into-AUCTeX t
 reftex-refontify-context t
 reftex-revisit-to-follow t
 reftex-save-parse-info t)

;; *** Preview

(d125q-customizeq
 preview-auto-cache-preamble t
 preview-fast-conversion nil
 preview-preserve-counters t)

;; ** Org

(d125q-customizeq
 org-image-actual-width nil)

(d125q-customizeq
 org-latex-compiler "lualatex"
 org-latex-bib-compiler "biber"
 org-latex-pdf-process '("latexmk -outdir=%o %f")
 org-preview-latex-process-alist '((lualatex->pdftocairo
                                    :programs ("lualatex" "pdftocairo")
                                    :description "`lualatex' -> `pdftocairo'"
                                    :message "Please install `lualatex' and `pdftocairo'."
                                    :image-input-type "pdf"
                                    :image-output-type "svg"
                                    :latex-compiler ("lualatex --interaction=nonstopmode --output-directory=%o %f")
                                    :image-converter ("pdftocairo -svg %f %O")))
 org-preview-latex-default-process 'lualatex->pdftocairo
 org-preview-latex-image-directory (file-name-as-directory
                                    (locate-user-emacs-file "ltximg"))
 org-latex-classes '(("article"
                      "\\documentclass[11pt]{article}"
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                     ("report"
                      "\\documentclass[11pt]{report}"
                      ("\\part{%s}" . "\\part*{%s}")
                      ("\\chapter{%s}" . "\\chapter*{%s}")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                     ("book"
                      "\\documentclass[11pt]{book}"
                      ("\\part{%s}" . "\\part*{%s}")
                      ("\\chapter{%s}" . "\\chapter*{%s}")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                     ("scrartcl"
                      "\\documentclass[version=last,fontsize=11pt,paper=A4,parskip=half]{scrartcl}"
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                     ("scrreprt"
                      "\\documentclass[version=last,fontsize=11pt,paper=A4,parskip=half]{scrreprt}"
                      ("\\part{%s}" . "\\part*{%s}")
                      ("\\chapter{%s}" . "\\chapter*{%s}")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                     ("scrbook"
                      "\\documentclass[version=last,fontsize=11pt,paper=A4,parskip=half]{scrbook}"
                      ("\\part{%s}" . "\\part*{%s}")
                      ("\\chapter{%s}" . "\\chapter*{%s}")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
 org-latex-default-class "scrartcl"
 org-latex-default-packages-alist '(("AUTO" "inputenc" t ("pdflatex"))
                                    ("T1" "fontenc" t ("pdflatex"))
                                    ("AUTO" "babel" t)
                                    ("babel,final" "microtype" t)
                                    ("svgnames,x11names" "xcolor" t)
                                    ("" "graphicx" t)
                                    ("" "adjustbox" nil)
                                    ("" "grffile" t)
                                    ("" "longtable" nil)
                                    ("" "wrapfig" nil)
                                    ("" "rotating" nil)
                                    ("normalem" "ulem" t)
                                    ("" "mathtools" t)
                                    ("" "amsthm" nil)
                                    ("warnings-off={mathtools-colon,mathtools-overbracket}" "unicode-math" t)
                                    ("newfloat" "minted" nil)
                                    ("" "booktabs" nil)
                                    ("inline" "enumitem" nil)
                                    ("" "capt-of" nil)
                                    ("unicode,breaklinks,colorlinks" "hyperref" nil))
 org-latex-packages-alist '()
 org-format-latex-header (mapconcat #'identity
                                    '("\\documentclass[11pt,preview]{standalone}"
                                      "[DEFAULT-PACKAGES]"
                                      "[PACKAGES]"
                                      "\\babelfont{rm}[Language=Default]{Libertinus Serif}"
                                      "\\babelfont{sf}[Language=Default]{Libertinus Sans}"
                                      "\\babelfont{tt}[Language=Default,Scale=MatchLowercase]{Latin Modern Mono}"
                                      "\\mathtoolsset{mathic}"
                                      "\\unimathsetup{math-style=ISO,bold-style=ISO}"
                                      "\\setmathfont[Language=Default]{Libertinus Math}")
                                    "\n")
 org-latex-default-figure-position ""
 org-latex-image-default-width "\\linewidth"
 org-latex-listings 'minted
 org-latex-minted-options '(("linenos" ""))
 org-latex-tables-booktabs t
 org-latex-prefer-user-labels t)

;; * Appearance

;; ** Startup

(d125q-customizeq
 inhibit-startup-echo-area-message (user-login-name)
 inhibit-startup-screen t
 initial-scratch-message nil)

;; ** Tooltips

(d125q-customizeq
 tooltip-mode t)

;; ** Matching parentheses

(d125q-customizeq
 show-paren-ring-bell-on-mismatch t
 show-paren-mode t)

;; ** Scroll bars

(d125q-customizeq
 scroll-bar-mode nil
 horizontal-scroll-bar-mode nil)

;; ** Mode line

(d125q-customizeq
 line-number-mode t
 column-number-mode t
 size-indication-mode t
 display-time-24hr-format t
 display-time-day-and-date t
 display-time-mode t
 display-battery-mode t
 minions-direct '(flymake-mode
                  flycheck-mode
                  flyspell-mode
                  reftex-mode)
 minions-mode t)

;; ** Theme

(d125q-customizeq
 modus-themes-fringes 'subtle
 modus-themes-paren-match 'intense
 modus-themes-prompts 'subtle-accented
 modus-themes-completions 'opinionated
 modus-themes-bold-constructs t
 modus-themes-slanted-constructs t
 modus-themes-mode-line '3d)

(modus-themes-load-vivendi)

;; ** Faces

(custom-set-faces
 '(fixed-pitch ((t (:inherit default))))
 '(fixed-pitch-serif ((t (:inherit default)))))

;; * Postamble

;; Local Variables:
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; time-stamp-start: "^;; Version: "
;; time-stamp-time-zone: t
;; time-stamp-format: "%Y%02m%02d%02H%02M%02S%"
;; time-stamp-end: "$"
;; End:

;;; init.el ends here
