;;; early-init.el --- d125q's early init file  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210319T103219+0100
;; Keywords: convenience

;;; Commentary:

;; The early init file is loaded very early in the startup process:
;; before graphical elements such as the tool bar are drawn, and
;; before the package manager is initialized.

;;; Code:

;; * Preamble

(push (locate-user-emacs-file "lisp" ".emacs-lisp") load-path)
(require 'd125q-utils)

(defvar secret-plist-file (locate-user-emacs-file "secret-plist.gpg" ".secret-plist.gpg")
  "File in which secret data is stored.")

(defvar secret-plist (when (file-exists-p secret-plist-file)
                       (with-temp-buffer
                         (insert-file-contents secret-plist-file)
                         (read (current-buffer))))
  "Decrypted contents of `secret-plist-file'.")

;; * Evaluation

(customize-variables
  eval-expression-debug-on-error nil
  user-emacs-directory-warning nil
  load-prefer-newer t)

;; * Personal data

(plist/customize-variables (secret-plist user)
  user-full-name
  user-mail-address)

(customize-variables
  plstore-encrypt-to user-mail-address)

;; * Visuals

(customize-variables
  ;; frame parameters
  default-frame-alist '((undecorated . nil)
                        (vertical-scroll-bars . nil)
                        (horizontal-scroll-bars . nil)
                        (menu-bar-lines . 0)
                        (tool-bar-lines . 0)
                        (border-width . 0)
                        (internal-border-width . 0)
                        (fullscreen . maximized))
  ;; resizing of mini frames and windows
  resize-mini-frames t
  resize-mini-windows 'grow-only
  ;; tool bars
  tool-bar-mode nil
  ;; dialog boxes
  use-dialog-box nil
  use-file-dialog nil
  ;; menu bars and prompts
  menu-bar-mode nil
  menu-prompting t
  ;; empty lines and buffer boundaries
  indicate-empty-lines t
  indicate-buffer-boundaries 'left
  ;; bell
  visible-bell t
  ;; X11-related things
  x-stretch-cursor t
  x-underline-at-descent-line t
  x-use-underline-position-properties t)

;; * Packages

(customize-variables
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

(customize-variables
  safe-local-variable-values '((TeX-command-extra-options . "-shell-escape")))

;; * History

(customize-variables
  history-delete-duplicates t
  history-length t)

;; * Postamble

;;; Local Variables:
;;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;;; time-stamp-start: "^;; Version: "
;;; time-stamp-format: "%Y%02m%02dT%02H%02M%02S%5z"
;;; time-stamp-end: "$"
;;; End:

;;; early-init.el ends here
