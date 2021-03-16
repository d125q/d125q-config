;;; d125q-helm-project.el --- d125q's Helm interface to `project.el'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 2021-03-14
;; Keywords: convenience

;;; Commentary:

;; This is a Helm interface to `project.el' providing Helm-powered
;; variants of common commands.

;;; Code:

;; * Preamble

(require 'helm)
(require 'helm-source)
(require 'helm-files)
(require 'helm-grep)
(require 'project)
(require 'magit)

;; * `helm-project-find-files'

(defclass hpff/source-class (helm-source-sync)
  ((init
    :initform (lambda ()
                (if-let* ((project (project-current))
                          (project-root (project-root project)))
                    (progn
                      (helm-set-attr 'candidates (project-files project))
                      (helm-set-attr 'header-line
                                     (format "%s | %s"
                                             project-root
                                             (helm-get-attr 'persistent-help)))
                      (setq helm-ff-default-directory project-root))
                  (user-error "Could not retrieve current project"))))
   (persistent-action-if
    :initform #'helm-find-files-persistent-action-if)
   (persistent-help
    :initform "Hit1 to expand candidate, Hit2 (or C-u) to find file")
   (help-message
    :initform helm-ff-help-message)
   (action
    :initform helm-find-files-actions)
   (action-transformer
    :initform #'helm-find-files-action-transformer)
   (keymap
    :initform helm-find-files-map)
   (candidate-number-limit
    :initform helm-ff-candidate-number-limit)
   (mode-line
    :initform (list "file(s)" helm-mode-line-string))
   (volatile
    :initform t)
   (migemo
    :initform t)
   (group
    :initform 'd125q-helm-project))
  :documentation "Source class for `helm-project-find-files'.")

(defvar hpff/source nil
  "Source for `helm-project-find-files'.")

(defun helm-project-find-files (_arg)
  "Find files in the current project using Helm."
  (interactive "P")
  (unless hpff/source
    (setq hpff/source (helm-make-source
                          "Find files in project" 'hpff/source-class)))
  (helm :sources 'hpff/source
        :case-fold-search helm-file-name-case-fold-search
        :prompt "Find file: "
        :buffer "*helm project find files*"))

;; * `helm-project-list-buffers'

(defclass hplb/source-class (helm-source-buffers)
  ((project
    :initarg :project
    :initform nil
    :custom (cons symbol string)
    :documentation "The current project for which buffers are being listed.")
   (init
    :initform (lambda ()
                (helm-aif (project-current)
                    (progn
                      (helm-set-attr 'project it)
                      (helm-set-attr 'header-line
                                     (format "%s | %s"
                                             (project-root it)
                                             (helm-get-attr 'persistent-help)))
                      (helm-buffers-list--init))
                  (user-error "Could not to retrieve current project"))))
   (candidate-transformer
    :initform (lambda (candidates)
                (helm-aif (helm-get-attr 'project)
                    (cl-loop
                     for buffer in candidates
                     when (equal it (with-current-buffer buffer (project-current)))
                     collect buffer)
                  (user-error "Invalid state; `project' is nil"))))
   (keymap
    :initform helm-buffer-map)
   (group :initform 'd125q-helm-project))
  :documentation "Source class for `helm-project-list-buffers'.")

(defvar hplb/source nil
  "Source for `helm-project-list-buffers'.")

(defun helm-project-list-buffers (_arg)
  "List buffers in the current project using Helm."
  (interactive "P")
  (unless hplb/source
    (setq hplb/source (helm-make-source
                          "Project buffers" 'hplb/source-class)))
  (helm :sources '(hplb/source helm-source-buffer-not-found)
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width
        :prompt "Switch to buffer: "
        :buffer "*helm project list buffers*"))

;; * `helm-project-grep'

(defun helm-project-grep (arg)
  "Grep the current project using Helm."
  (interactive "P")
  (helm-aif (project-current)
      (helm-grep-ag (expand-file-name (project-root it)) arg)
    (user-error "Could not retrieve the current project")))

;; * `helm-project-switch-project'

;; ** :action

(defun hpsp/project--remove-from-project-list (_project)
  "Remove all marked projects from the project list."
  (with-helm-buffer
    (mapc #'project--remove-from-project-list (helm-marked-candidates))
    (helm-force-update)))

(defmacro hpsp/define-actions (&rest spec)
  "Define actions for HPSP out of SPEC."
  (let (defuns values action)
    (while spec
      (cl-destructuring-bind (name command &key skip-defun) (pop spec)
        (setq action (intern (format "hpsp/%s" command)))
        (unless skip-defun
          (push `(defun ,action (project)
                   ,(format "Run `%s' in PROJECT." command)
                   (let ((default-directory project)
                         (project-current-inhibit-prompt t))
                     (call-interactively #',command)))
                defuns))
        (push `(cons ,name ',action) values)))
    `(progn
       ,@(nreverse
          (cons `(defvar hpsp/actions
                   (list ,@(nreverse values))
                   "Actions for `helm-project-switch-project'.")
                defuns)))))

(hpsp/define-actions
 ("Magit status" magit-status)
 ("VC-Dir" vc-dir)
 ("Dired" dired)
 ("Find files" helm-project-find-files)
 ("List buffers" helm-project-list-buffers)
 ("Shell" project-shell)
 ("Eshell" project-eshell)
 ("Find regexp" project-find-regexp)
 ("Remove from project list" project--remove-from-project-list :skip-defun t))

;; ** :action-transformer

(defun hpsp/action-transformer (actions candidate)
  (if (magit-toplevel candidate)
      actions
    (rassq-delete-all 'magit-status actions)))

;; ** :persistent-action-if

(defun hpsp/persistent-action-if (candidate)
  "Persistent action for `helm-project-switch-project'.

It runs `magit-status' if possible, else `vc-dir'.  If a prefix
argument was supplied, `vc-dir' is run even when `magit-status'
is possible."
  (if (or current-prefix-arg (not (magit-toplevel candidate)))
      'vc-dir
    'magit-status))

;; ** :keymap

(defun hpsp/run-project--remove-from-project-list ()
  "Run `project--remove-from-project-list' with a key binding."
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'project--remove-from-project-list
                   '(hpsp/project--remove-from-project-list . never-split))
    (helm-execute-persistent-action 'project--remove-from-project-list)))
(put 'hpsp/run-project--remove-from-project-list 'helm-only t)

(defmacro hpsp/define-keymap (&rest spec)
  "Define a keymap for HPSP out of SPEC."
  (let (defuns props kbds runner action)
    (while spec
      (cl-destructuring-bind (key command &key skip-defun) (pop spec)
        (setq runner (intern (format "hpsp/run-%s" command))
              action (intern (format "hpsp/%s" command)))
        (unless skip-defun
          (push `(defun ,runner ()
                           ,(format "Run `%s' with a key binding." command)
                           (interactive)
                           (with-helm-alive-p
                             (helm-exit-and-execute-action ',action)))
                defuns)
          (push `(put ',runner 'helm-only t) props))
        (push `(define-key map (kbd ,key) ',runner) kbds)))
    `(progn
       ,@(nreverse
          (cons `(defvar hpsp/map
                   (let ((map (make-sparse-keymap)))
                     ,@(nreverse kbds)
                     map)
                   "Keymap for `helm-project-switch-project'.")
                (nconc props defuns))))))

(hpsp/define-keymap
 ("C-x g" magit-status)
 ("C-x v d" vc-dir)
 ("C-x d" dired)
 ("C-x C-f" helm-project-find-files)
 ("C-x b" helm-project-list-buffers)
 ("C-c C-s" project-shell)
 ("C-c C-e" project-eshell)
 ("M-s" project-find-regexp)
 ("M-D" project--remove-from-project-list :skip-defun t))

;; ** Main function with associated data

(defclass hpsp/source-class (helm-source-sync)
  ((init :initform (lambda ()
                     (project--ensure-read-project-list)
                     (helm-set-attr 'candidates project--list)))
   (action :initform 'hpsp/actions)
   (action-transformer :initform 'hpsp/action-transformer)
   (persistent-action-if :initform 'hpsp/persistent-action-if)
   (persistent-help :initform "Magit if possible, else (C-u to force) VC-Dir")
   (keymap :initform hpsp/map)
   (volatile :initform t)
   (migemo :initform t)
   (group :initform 'd125q-helm-project)))

(defvar hpsp/source nil
  "Source for `helm-project-switch-project'.")

(defun helm-project-switch-project ()
  (interactive)
  (unless hpsp/source
    (setq hpsp/source (helm-make-source
                          "Switch project" 'hpsp/source-class)))
  (helm :sources 'hpsp/source
        :prompt "Switch to project: "
        :buffer "*helm project switch project*"))

;; * Postamble

(provide 'd125q-helm-project)

;;; d125q-helm-project.el ends here
