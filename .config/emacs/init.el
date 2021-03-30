;;; init.el --- d125q's init file  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210330T155444+0200
;; Keywords: convenience

;;; Commentary:

;; When GNU Emacs is started, it normally tries to load a Lisp program
;; from an initialization file, or init file for short.  This file, if
;; it exists, specifies how to initialize GNU Emacs.

;;; Code:

;; * Preamble

(require 'd125q-utils)
(defvar secret-plist)                 ; defined in the early init file

(define-controls show-trailing-whitespace)
(define-controls indent-tabs-mode)

;; * Magit

(customize-variables
  magit-define-global-key-bindings t
  magit-completing-read-function 'magit-ido-completing-read)

(require 'magit)
(require 'magit-extras)

;; * Minibuffer

(customize-variables
  enable-recursive-minibuffers t
  minibuffer-auto-raise t
  minibuffer-eldef-shorten-default t
  minibuffer-depth-indicate-mode t
  minibuffer-electric-default-mode t
  savehist-mode t)

;; ** Icomplete

(defun icomplete-fido-updir-or-backward-kill-sexp ()
  "Go up one directory or kill the preceding S-expression."
  (interactive)
  (declare-function icomplete--category "icomplete")
  (if (eq (icomplete--category) 'file)
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-kill-sexp)))

(customize-variables
  icomplete-prospects-height 2
  icomplete-show-matches-on-no-input t)

(define-key-bindings (:map icomplete-fido-mode-map :pkg icomplete)
  ("C-l" icomplete-fido-updir-or-backward-kill-sexp))

;; ** Ido

(customize-variables
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

;; `magit-extras' has already been loaded
(define-key-bindings (:map ido-common-completion-map :pkg ido)
  ("C-x g" ido-enter-magit-status :nodecl t))

(autoload 'ido-everywhere "ido")
(ido-everywhere)
(amx-mode)

;; * Helm

(customize-variables
  helm-display-function #'helm-default-display-buffer
  helm-command-prefix-key nil
  helm-ff-DEL-up-one-level-maybe t
  helm-ff-display-image-native t
  helm-buffer-max-length 40
  helm-buffer-end-truncated-string "…"
  helm-describe-function-function #'helpful-function
  helm-describe-variable-function #'helpful-variable
  helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")

(require 'helm-config)
(require 'helm-files)

(defun helm-ff-magit-status (candidate)
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

(defun helm-ff-run-magit-status ()
  "Open the Magit status from Helm FF with a key binding."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ff-magit-status)))
(put 'helm-ff-run-magit-status 'helm-only t)

(cl-pushnew '("Open the Magit status" . helm-ff-magit-status)
            (cdr (last helm-find-files-actions)))

(define-key-bindings (:map helm-find-files-map)
  ("C-x g" helm-ff-run-magit-status))

(define-key-bindings (:prepend "C-c")
  ("c" helm-resume)
  ("x" helm-M-x)
  ("f" helm-find-files)
  ("F" helm-recentf)
  ("y" helm-show-kill-ring)
  ("b" helm-buffers-list)
  ("B" helm-filtered-bookmarks)
  ("i" helm-imenu)
  ("I" helm-imenu-in-all-buffers)
  ("s" helm-semantic)
  ("r" helm-register)
  ("m" helm-mini)
  ("M" helm-multi-files))

(define-key-bindings ()
  ("s-SPC" helm-all-mark-rings)
  ("s-:" helm-eval-expression-with-eldoc)
  ("s-/" helm-dabbrev))

(define-key-bindings (:prefix "C-c h")
  ("a" helm-apropos)
  ("g" helm-info-gnus)
  ("i" helm-info-at-point)
  ("m" helm-man-woman)
  ("r" helm-info-emacs))

(define-key-bindings (:prefix "C-c s")
  ("e" helm-etags-select)
  ("f" helm-find)
  ("g" helm-do-grep-ag)
  ("G" helm-grep-do-git-grep)
  ("l" helm-locate)
  ("o" helm-occur)
  ("r" helm-regexp))

;; * Files

;; ** Auto-save files

(defvar auto-save-directory (file-name-as-directory
                             (locate-user-emacs-file "auto-save-files"))
  "Default directory for auto-save files.")

(customize-variables
  auto-save-file-name-transforms `((".*" ,auto-save-directory t))
  auto-save-default t
  auto-save-no-message t
  delete-auto-save-files t)

;; ** Backup files

(defvar backup-directory (file-name-as-directory
                          (locate-user-emacs-file "backup-files"))
  "Default directory for backup files.")

(customize-variables
  backup-directory-alist `((".*" . ,backup-directory))
  tramp-backup-directory-alist backup-directory-alist
  make-backup-files t
  vc-make-backup-files t
  version-control t
  backup-by-copying t
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2)

;; ** Operations on files

(customize-variables
  delete-by-moving-to-trash t     ; use the system's trash can
  file-precious-flag t            ; protect against I/O errors
  break-hardlink-on-save t        ; allow breaking of hard links
  vc-follow-symlinks t)           ; follow symbolic links to files under VC

;; ** Auto-revert

(customize-variables
  auto-revert-check-vc-info t
  auto-revert-verbose nil
  global-auto-revert-mode t)

;; ** Save-place

(customize-variables
  save-place-limit nil
  save-place-version-control t
  save-place-mode t)

;; ** Recent files

(defun recentf-find-file ()
  "Find a recent file."
  (interactive)
  (defvar recentf-list)
  (if-let* ((alist (mapcar (lambda (fn)
                             (cons (abbreviate-file-name fn) fn))
                           recentf-list))
            (keys (mapcar #'car alist))
            (default (car keys)))
      (let* ((key (completing-read "Find recent file: " keys nil t nil nil default))
             (entry (assoc key alist #'string=)))
        (find-file (cdr entry)))
    (user-error "No recent files to choose from")))

(customize-variables
  recentf-max-saved-items nil
  recentf-mode t)

(define-key-bindings (:map recentf-mode-map)
  ("s-r" recentf-open-files)
  ("s-R" recentf-find-file))

;; * `server.el'

;; for some reason GNOME ignores the `raise-frame' in
;; `server-switch-buffer' and instead only displays a "GNU Emacs is
;; ready" notification, so we use `wmctrl' on top of it
(advice-add 'server-switch-buffer :after (lambda (&rest _)
                                           (when server-raise-frame
                                             (wmctrl-raise-frame))))

;; * Dired

(customize-variables
  dired-always-read-filesystem t
  dired-auto-revert-buffer t
  dired-create-destination-dirs 'ask
  dired-kept-versions 2
  dired-vc-rename-file t)

;; * Helpful

(define-key-bindings (:prefix "s-h")
  ("c" helpful-callable)
  ("f" helpful-function)
  ("p" helpful-at-point)
  ("i" helpful-command)
  ("k" helpful-key)
  ("m" helpful-macro)
  ("s" helpful-symbol)
  ("v" helpful-variable))

;; * Editing

;; ** Killing

(customize-variables
  kill-read-only-ok t
  kill-ring-max 1024
  save-interprogram-paste-before-kill t)

(browse-kill-ring-default-keybindings)

(define-key-bindings ()
  ("s-k" kill-whole-line))

;; ** Isearch

(customize-variables
  search-ring-max 128
  regexp-search-ring-max 128
  isearch-lazy-count t
  isearch-lazy-highlight t)

;; ** `grep' and friends

(autoload 'vc-git-grep "vc-git")

(define-key-bindings (:map search-map :prefix "g")
  ("g" grep)
  ("v" vc-git-grep)
  ("f" grep-find)
  ("l" lgrep)
  ("r" rgrep)
  ("z" zrgrep))

(customize-variables
  rg-keymap-prefix (kbd "s-g")
  rg-use-transient-menu t)

(rg-enable-default-bindings)

(define-key-bindings (:map search-map)
  ("d" deadgrep)
  ("l" locate))

;; ** Jumping to things

(customize-variables
  avy-background nil)

(avy-setup-default)
(ace-link-setup-default)

(define-key-bindings ()
  ("s-'" avy-goto-char-timer)
  ("s-<return>" ace-window))

;; ** Iedit

(customize-variables
  iedit-toggle-key-default (kbd "s-;"))

(require 'iedit)

;; ** Imenu

(customize-variables
  imenu-auto-rescan t
  imenu-use-popup-menu nil)

(define-key-bindings ()
  ("s-i" imenu))

;; ** `expand-region'

(define-key-bindings ()
  ("s-=" er/expand-region))

;; ** `multiple-cursors'

(mapc (lambda (fn)
        (autoload fn "mc-mark-more"))
      '(mc/mark-next-like-this-symbol
        mc/mark-previous-like-this-symbol))

(define-transient-map
    (:exit-key "M" :persist-by-default t) "multiple-cursors" "s-M"
  ("l" mc/mark-next-like-this)
  ("h" mc/mark-previous-like-this)
  ("w" mc/mark-next-word-like-this)
  ("b" mc/mark-previous-word-like-this)
  ("W" mc/mark-next-symbol-like-this)
  ("B" mc/mark-previous-symbol-like-this)
  ("SPC w" mc/mark-next-like-this-word)
  ("SPC b" mc/mark-previous-like-this-word)
  ("SPC B" mc/mark-previous-like-this-symbol)
  ("SPC W" mc/mark-next-like-this-symbol)
  ("j" mc/unmark-next-like-this)
  ("k" mc/unmark-previous-like-this)
  ("J" mc/skip-to-next-like-this)
  ("K" mc/skip-to-previous-like-this))

(with-eval-after-package
    multiple-cursors-core t (mc/cmds-to-run-once) nil
  (cl-pushnew 'activate-multiple-cursors-transient-map mc/cmds-to-run-once)
  (cl-pushnew 'deactivate-transient-map mc/cmds-to-run-once))

(define-key-bindings ()
  ("s-a" mc/mark-all-like-this)
  ("s-A w" mc/mark-all-words-like-this)
  ("s-A W" mc/mark-all-symbols-like-this)
  ("s-s" mc/mark-all-in-region)
  ("s-S" mc/mark-all-in-region-regexp)
  ("s-d" mc/mark-all-like-this-in-defun)
  ("s-D w" mc/mark-all-words-like-this-in-defun)
  ("s-D W" mc/mark-all-symbols-like-this-in-defun))

;; * Ibuffer

(customize-variables
  ibuffer-eliding-string "…")

(define-key-bindings ()
  ("s-b" ibuffer))

;; * `which-key'

(customize-variables
  which-key-allow-imprecise-window-fit nil
  which-key-compute-remap t
  which-key-show-transient-maps t
  which-key-mode t)

;; * Speedbar

(defun speedbar-frame-set-input-focus ()
  "Select SPEEDBAR-FRAME, raise and, and set input focus."
  (when (and (boundp 'speedbar-frame) speedbar-frame)
    (select-frame-set-input-focus speedbar-frame)))

(customize-variables
  speedbar-frame-parameters '((undecorated . t)
                              (fullscreen . nil)
                              (minibuffer . nil)
                              (width . 40)
                              (unsplittable . t))
  speedbar-after-create-hook (list #'speedbar-frame-reposition-smartly
                                   #'speedbar-frame-set-input-focus)
  speedbar-vebosity-level 0)

(define-key-bindings ()
  ("<f9>" speedbar))

;; * The Customization interface

(customize-variables
  custom-file (locate-user-emacs-file "custom.el")
  custom-buffer-done-kill t
  custom-buffer-verbose-help nil
  custom-raised-buttons t
  custom-search-field nil
  custom-unlispify-tag-names nil
  custom-variable-default-form 'lisp)

(define-key-bindings (:prefix "s-c")
  ("a" customize-apropos)
  ("f" customize-face)
  ("4 f" customize-face-other-window)
  ("F" customize-apropos-faces)
  ("g" customize-group)
  ("4 g" customize-group-other-window)
  ("G" customize-apropos-groups)
  ("o" customize-option)
  ("4 o" customize-option-other-window)
  ("O" customize-apropos-options)
  ("v" customize-variable)
  ("4 v" customize-variable-other-window)
  ("t" customize-themes)
  ("r" customize-rogue)
  ("s" customize-saved)
  ("u" customize-unsaved)
  ("c" customize-customized)
  ("n" customize-changed))

;; * Comint

(customize-variables
  comint-prompt-read-only t)

(define-key-bindings (:map comint-mode-map :pkg comint)
  ([remap kill-whole-line] comint-kill-whole-line)
  ([remap kill-region] comint-kill-region))

;; * `project.el'

(autoload 'project-root "project")

(with-eval-after-package
    project nil (project-prefix-map project-switch-commands) nil
  (require 'd125q-helm-project)
  (define-key-bindings (:prepend "C-c")
    ("p" helm-project-switch-project))
  (define-key-bindings (:map project-prefix-map)
    ("F" helm-project-find-files)
    ("B" helm-project-list-buffers)
    ("G" helm-project-grep))
  (setq project-switch-commands
        (append project-switch-commands
                '((helm-project-find-files "Find files (Helm)")
                  (helm-project-list-buffers "List buffers (Helm)")
                  (helm-project-grep "grep (Helm)")))))

;; * Mail and News

(customize-variables
  gnus-completing-read-function 'gnus-ido-completing-read
  gnus-dired-mail-mode 'gnus-user-agent
  gnus-picon-style 'right
  compose-mail-user-agent-warnings nil
  mail-user-agent 'gnus-user-agent
  mail-use-rfc822 t
  mm-discouraged-alternatives '("text/html" "text/richtext")
  mm-text-html-renderer 'gnus-w3m)

(plist/customize-variables (secret-plist mail)
  send-mail-function
  message-send-mail-function
  smtpmail-default-smtp-server
  smtpmail-smtp-service
  smtpmail-smtp-user)

(plist/customize-variables (secret-plist gnus)
  gnus-select-method
  gnus-secondary-select-methods)

;; * IRC

(plist/customize-variables (secret-plist erc)
  erc-nick
  erc-server
  erc-port
  erc-user-full-name
  erc-nickserv-passwords)

(plist/set-variables (secret-plist erc :pkg erc)
  erc-password)

(customize-variables
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

(customize-variables
  prog-mode-hook (list #'turn-on-show-trailing-whitespace
                       #'display-line-numbers-mode
                       #'flyspell-prog-mode)
  company-show-numbers t
  company-tooltip-align-annotations t
  company-tooltip-flip-when-above t
  global-company-mode t)

(define-transient-map
    (:map flymake-mode-map :pkg flymake) "flymake" "C-c !"
  ("n" flymake-goto-next-error :persist t)
  ("p" flymake-goto-prev-error :persist t)
  ("s" flymake-start)
  ("l" flymake-switch-to-log-buffer)
  ("d" flymake-show-diagnostics-buffer)
  ("r" flymake-running-backends)
  ("D" flymake-disabled-backends)
  ("R" flymake-reporting-backends))

(with-eval-after-package
    multiple-cursors-core t (mc/cmds-to-run-once) nil
  (cl-pushnew 'activate-flymake-transient-map mc/cmds-to-run-once))

;; ** Emacs Lisp

(defun emacs-lisp-outline-level ()
  "Compute the outline level for Emacs Lisp code."
  (defvar outline-heading-alist)
  (or (cdr (assoc (match-string 0) outline-heading-alist))
      (- (match-end 1) (match-beginning 1))))

(defun emacs-lisp-setup-outline ()
  "Setup outlines for Emacs Lisp code."
  (declare-function outline-hide-sublevels "outline")
  (setq-local outline-regexp ";; \\(\\*+\\) .*$"
              outline-level #'emacs-lisp-outline-level)
  (outline-minor-mode)
  (outline-hide-sublevels 1))

(customize-variables
  flycheck-emacs-lisp-load-path 'inherit
  flycheck-emacs-lisp-initialize-packages t)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-setup-outline)

;; ** Python

(customize-variables
  python-indent-guess-indent-offset-verbose nil
  python-shell-prompt-detect-failure-warning nil)

(define-key-bindings (:map pyvenv-mode-map :prefix "s-v" :pkg pyvenv :ext-p t)
  ("a" pyvenv-activate)
  ("w" pyvenv-workon)
  ("d" pyvenv-deactivate)
  ("c" pyvenv-create))

(customize-variables
  pyvenv-mode t
  pyvenv-tracking-mode t)

(add-hook 'python-mode-hook #'eglot-ensure)

;; ** R and friends (via ESS)

(customize-variables
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

(defvar project-root->sql-product '(("Exasol" . ansi)
                                    ("Oracle" . oracle))
  "Alist mapping project root to SQL product.")

(defun set-sql-product-from-project ()
  "Set `sql-product' according to the current project."
  (when-let* ((project (project-current))
              (project-root (project-root project))
              (key (file-name-nondirectory (directory-file-name project-root)))
              (product (cdr (assoc key project-root->sql-product #'string=))))
    (message "Setting `sql-product' to `%s' in %s" product project-root)
    (setq-local sql-product product)))

(mapc (lambda (extension)
        (cl-pushnew (cons (format "\\.%s\\'" extension) 'sql-mode)
                    auto-mode-alist))
      '("fnc" "pck" "prc"))

(customize-variables
  sql-product 'ansi
  sql-mode-hook (list #'turn-off-indent-tabs-mode
                      #'turn-off-show-trailing-whitespace
                      #'set-sql-product-from-project
                      #'sql-indent-enable))

(plist/customize-variables (secret-plist sql)
  sql-connection-alist)

;; ** Shell scripts

(defun sh-outline-level ()
  "Compute the outline level for Shell scripts."
  (defvar outline-heading-alist)
  (or (cdr (assoc (match-string 0) outline-heading-alist))
      (- (match-end 1) (match-beginning 1))))

(defun sh-setup-outline ()
  "Setup outlines for Shell scripts."
  (declare-function outline-hide-sublevels "outline")
  (setq-local outline-regexp "## \\(\\*+\\).*$"
              outline-level #'sh-outline-level)
  (outline-minor-mode)
  (outline-hide-sublevels 1))

(add-hook 'sh-mode-hook #'sh-setup-outline)

;; * Editing human-readable text

(customize-variables
  text-mode-hook (list #'turn-on-flyspell
                       #'turn-on-auto-fill
                       #'display-line-numbers-mode
                       #'visual-line-mode
                       #'text-mode-hook-identify))

(customize-variables
  flyspell-issue-message-flag nil
  flyspell-issue-welcome-flag nil
  flyspell-use-meta-tab nil
  ispell-highlight-face 'flyspell-incorrect)

;; ** TeX and friends

;; *** TeX

(customize-variables
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
  TeX-view-program-list
  '(("Zathura (Fullscreen)"
     ("zathura --mode fullscreen %o"
      (mode-io-correlate " --synctex-forward %n:0:\"%b\""))
     "zathura"))
  TeX-view-program-selection
  '(((output-dvi has-no-display-manager) "dvi2tty")
    ((output-dvi style-pstricks) "dvips and gv")
    (output-dvi "xdvi")
    (output-pdf "Zathura (Fullscreen)")
    (output-html "xdg-open")))

(autoload 'TeX-revert-document-buffer "tex-buf")
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; *** LaTeX

(customize-variables
  LaTeX-Omega-command "lamed"
  LaTeX-default-style "scrartcl"
  LaTeX-default-options '("version=last" "paper=A4")
  LaTeX-electric-left-right-brace nil
  LaTeX-enable-toolbar nil
  LaTeX-mode-hook (list #'LaTeX-math-mode
                        #'reftex-mode))

;; *** BibTeX

(customize-variables
  bibtex-dialect 'biblatex
  bibtex-entry-format t
  bibtex-field-kill-ring-max 128
  bibtex-maintain-sorted-entries 'entry-class
  bibtex-parse-keys-fast nil)

;; *** RefTeX

(customize-variables
  reftex-plug-into-AUCTeX t
  reftex-refontify-context t
  reftex-revisit-to-follow t
  reftex-save-parse-info t)

;; *** Preview

(customize-variables
  preview-auto-cache-preamble t
  preview-fast-conversion nil
  preview-preserve-counters t)

;; ** Org

(customize-variables
  org-image-actual-width nil)

(customize-variables
  org-latex-compiler "lualatex"
  org-latex-bib-compiler "biber"
  org-latex-pdf-process '("latexmk -outdir=%o %f")
  org-preview-latex-process-alist
  '((lualatex->pdftocairo
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
  org-latex-classes
  '(("article"
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
  org-latex-default-packages-alist
  '(("AUTO" "inputenc" t ("pdflatex"))
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
  org-format-latex-header
  (mapconcat #'identity
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

(customize-variables
  inhibit-startup-echo-area-message (user-login-name)
  inhibit-startup-screen t
  initial-scratch-message nil)

;; ** Tooltips

(customize-variables
  tooltip-mode t)

;; ** Matching parentheses

(customize-variables
  show-paren-ring-bell-on-mismatch t
  show-paren-mode t)

;; ** Scroll bars

(customize-variables
  scroll-bar-mode nil
  horizontal-scroll-bar-mode nil)

;; ** Mode line

(customize-variables
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

(customize-variables
  modus-themes-fringes 'subtle
  modus-themes-paren-match 'intense
  modus-themes-prompts 'subtle-accented
  modus-themes-completions 'opinionated
  modus-themes-bold-constructs t
  modus-themes-slanted-constructs t
  modus-themes-mode-line '3d)

(modus-themes-load-vivendi)

;; ** Faces

(customize-faces
 (t
  (fixed-pitch (:inherit default))
  (fixed-pitch-serif (:inherit default))))

;; * Postamble

;;; Local Variables:
;;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;;; time-stamp-start: "^;; Version: "
;;; time-stamp-format: "%Y%02m%02dT%02H%02M%02S%5z"
;;; time-stamp-end: "$"
;;; End:

;;; init.el ends here
