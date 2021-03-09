;;; d125q-helm-project.el --- d125q's Helm interface to `project.el'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 2021-03-09
;; Keywords: convenience

;;; Commentary:

;; This is a Helm interface to `project.el' providing Helm-powered
;; variants of common commands.

;;; Code:

;; * Preamble

(require 'project)
(require 'helm)
(require 'helm-source)
(require 'helm-files)
(require 'helm-grep)

;; * `helm-project-find-files'

(defvar helm-project-ff-candidates nil
  "Candidates for `helm-project-find-files'.")

(defvar helm-project-ff-source nil
  "Source for `helm-project-find-files'.")

(defclass helm-project-ff-source-class (helm-source-sync)
  ((init :initform (lambda ()
                     (helm-set-attr 'candidates helm-project-ff-candidates)))
   (persistent-action-if :initform #'helm-find-files-persistent-action-if)
   (persistent-help :initform "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
   (help-message :initform 'helm-hpff-help-message)
   (action :initform 'helm-find-files-actions)
   (action-transformer :initform 'helm-find-files-action-transformer)
   (mode-line :initform (list "File(s)" helm-mode-line-string))
   (volatile :initform t)
   (migemo :initform t)
   (nohighlight :initform t)
   (group :initform 'd125q-helm-project)))

;;;###autoload
(defun helm-project-find-files ()
  "Find files in the current project using Helm."
  (interactive)
  (if-let (project (project-current))
      (let ((helm-ff-default-directory (project-root project))
            (helm-project-ff-candidates (project-files project)))
        (unless helm-project-ff-source
          (setq helm-project-ff-source (helm-make-source
                                       "Project FF" 'helm-project-ff-source-class)))
        (helm :sources 'helm-project-ff-source
              :case-fold-search helm-file-name-case-fold-search
              :prompt "Find files in project: "
              :buffer "*helm project find files*"))
    (user-error "Could not retrieve the current project")))

;; * `helm-project-buffers-list'

(defvar helm-project-bl-project nil
  "Project for `helm-project-buffers-list'.")

(defvar helm-project-bl-source nil
  "Source for `helm-project-buffers-list'.")

(defclass helm-project-bl-source-class (helm-source-buffers)
  ((candidate-transformer
    :initform (lambda (candidates)
                (cl-loop
                 for buf in candidates
                 when (equal helm-project-bl-project
                             (with-current-buffer buf (project-current)))
                 collect buf)))
   (group :initform 'd125q-helm-project)))

;;;###autoload
(defun helm-project-buffers-list ()
  "List buffers in the current project using Helm."
  (interactive)
  (if-let (helm-project-bl-project (project-current))
      (progn
        (message "%s" helm-project-bl-project)
        (unless helm-project-bl-source
          (setq helm-project-bl-source (helm-make-source
                                       "Project BL" 'helm-project-bl-source-class)))
        (helm :sources '(helm-project-bl-source
                         helm-source-buffer-not-found)
              :keymap helm-buffer-map
              :truncate-lines helm-buffers-truncate-lines
              :left-margin-width helm-buffers-left-margin-width
              :prompt "Switch to buffer in project: "
              :buffer "*helm project buffers*"))
    (user-error "Could not retrieve the current project")))

;; `helm-project-grep'

;;;###autoload
(defun helm-project-grep (arg)
  "Grep the current project using Helm."
  (interactive "P")
  (if-let (project (project-current))
      (helm-grep-ag (expand-file-name (project-root project)) arg)
    (user-error "Could not retrieve the current project")))

;; * Postamble

(provide 'd125q-helm-project)

;;; d125q-helm-project.el ends here
