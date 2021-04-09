;;; early-init.el --- d125q's early init file  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210409095142
;; Keywords: convenience

;;; Commentary:

;; The early init file is loaded very early in the startup process:
;; before graphical elements such as the tool bar are drawn, and
;; before the package manager is initialized.

;;; Code:

;; * Preamble

(require 'cl-lib)
(cl-pushnew (locate-user-emacs-file "lisp") load-path)
(require 'd125q-lib)

(defvar d125q-secret-plist-file (locate-user-emacs-file "secret-plist.gpg")
  "File in which secret data is stored.")

(defvar d125q-secret-plist (when (file-exists-p d125q-secret-plist-file)
                             (with-temp-buffer
                               (insert-file-contents d125q-secret-plist-file)
                               (read (current-buffer))))
  "Decrypted contents of `d125q-secret-plist-file'.")

;; * Evaluation

(d125q-customizeq
 eval-expression-debug-on-error nil
 load-prefer-newer t
 ad-redefinition-action 'accept
 user-emacs-directory-warning nil)

;; * Startup

(d125q-customizeq
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message (user-login-name)
 initial-scratch-message nil
 initial-major-mode 'fundamental-mode)

;; * Personal data

(d125q-plist-customizeq (d125q-secret-plist :user)
  user-full-name
  user-mail-address)

(d125q-customizeq
 plstore-encrypt-to user-mail-address)

;; * Appearance

;; ** Frames

(d125q-customizeq
 default-frame-alist '((undecorated . nil)
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (menu-bar-lines . 0)
                       (tool-bar-lines . 0)
                       (border-width . 0)
                       (internal-border-width . 0)
                       (fullscreen . maximized))
 frame-inhibit-implied-resize t)

;; ** Windows

(d125q-customizeq
 highlight-nonselected-windows nil
 cursor-in-non-selected-windows nil
 fast-but-imprecise-scrolling t
 redisplay-skip-fontification-on-input t)

;; ** Mini frames and windows

(d125q-customizeq
 resize-mini-frames t
 resize-mini-windows 'grow-only)

;; ** Tool bars

(d125q-customizeq
 tool-bar-mode nil)

;; ** Dialog boxes

(d125q-customizeq
 use-dialog-box nil
 use-file-dialog nil)

;; ** Menu bars and prompts

(d125q-customizeq
 menu-bar-mode nil
 menu-prompting t)

;; ** Empty lines and buffer boundaries

(d125q-customizeq
 indicate-empty-lines t
 indicate-buffer-boundaries 'left)

;; ** Bell

(d125q-customizeq
 visible-bell t)

;; ** X11-related things

(d125q-customizeq
 x-stretch-cursor t
 x-underline-at-descent-line t
 x-use-underline-position-properties t)

;; * Packages

(d125q-customizeq
 async-bytecomp-allowed-packages nil
 package-selected-packages '(ace-link
                             ace-window
                             ack
                             ada-mode
                             ada-ref-man
                             ag
                             amx
                             arduino-mode
                             auctex
                             avy
                             browse-kill-ring
                             cider
                             clojure-mode
                             company
                             csv-mode
                             deadgrep
                             ediprolog
                             eglot
                             erlang
                             ess
                             expand-region
                             flycheck
                             geiser
                             ggtags
                             git-link
                             groovy-mode
                             haskell-mode
                             helm
                             helm-ls-git
                             helm-ls-svn
                             helpful
                             htmlize
                             ibuffer-project
                             ido-completing-read+
                             iedit
                             js2-mode
                             json-mode
                             julia-mode
                             lua-mode
                             magit
                             markdown-mode
                             merlin
                             minions
                             modus-themes
                             multiple-cursors
                             org-plus-contrib
                             pdf-tools
                             pyvenv
                             rg
                             sbt-mode
                             scala-mode
                             slime
                             sml-mode
                             sql-indent
                             tuareg
                             w3m
                             wgrep
                             wgrep-ack
                             wgrep-ag
                             which-key
                             yaml-mode
                             yasnippet)
 package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("org" . "https://orgmode.org/elpa/"))
 package-quickstart t)

;; * Safe commands, variables, and expressions

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(d125q-customizeq
 safe-local-variable-values '((TeX-command-extra-options . "-shell-escape")))

;; * History

(d125q-customizeq
 history-delete-duplicates t
 history-length t)

;; * Postamble

;; Local Variables:
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; time-stamp-start: "^;; Version: "
;; time-stamp-time-zone: t
;; time-stamp-format: "%Y%02m%02d%02H%02M%02S%"
;; time-stamp-end: "$"
;; End:

;;; early-init.el ends here
