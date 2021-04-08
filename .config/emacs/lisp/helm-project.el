;;; helm-project.el --- Helm interface to `project.el'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210407141839
;; Keywords: convenience

;;; Commentary:

;; This is an interface to `project.el' which provides Helm-powered
;; commands to find files, list buffers and grep in projects.  It also
;; provides `helm-project-switch-project' to facilitate switching
;; between projects and executing commands against them.

;;; Code:

;; * Preamble

(require 'helm-files)
(require 'project)

(autoload 'magit-toplevel "magit-git")
(autoload 'vc-git-grep "vc-git")

;; * `helm-project-find-files'

(defclass helm-project--ff-source-class (helm-source-sync)
  ((init
    :initform (lambda ()
                (if-let* ((project (project-current))
                          (project-root (project-root project)))
                    (let* ((real (project-files project))
                           (cpd (file-name-directory
                                 (try-completion "" real)))
                           (cpd-len (length cpd))
                           (display (mapcar
                                     (lambda (r) (substring r cpd-len))
                                     real)))
                      (helm-set-attr 'candidates
                                     (cl-mapcar #'cons display real))
                      (helm-set-attr 'header-line
                                     (format "%s | %s"
                                             project-root
                                             (helm-get-attr 'persistent-help)))
                      (setq helm-ff-default-directory project-root))
                  (user-error "Could not retrieve current project"))))
   (persistent-action
    :initform (lambda (candidate)
                (funcall helm-ff-kill-or-find-buffer-fname-fn candidate)))
   (persistent-help
    :initform "View candidate or kill its buffer")
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
    :initform 'helm-project))
  :documentation "Source class for `helm-project-find-files'.")

(defvar helm-project--ff-source nil
  "Source for `helm-project-find-files'.")

;;;###autoload
(defun helm-project-find-files (_arg)
  "Find files in the current project using Helm."
  (interactive "P")
  (unless helm-project--ff-source
    (setq helm-project--ff-source (helm-make-source
                           "Find files in project" 'helm-project--ff-source-class)))
  (helm :sources 'helm-project--ff-source
        :case-fold-search helm-file-name-case-fold-search
        :prompt "Find file: "
        :buffer "*helm project find files*"))

;; * `helm-project-list-buffers'

(defclass helm-project--lb-source-class (helm-source-buffers)
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
   (group
    :initform 'helm-project))
  :documentation "Source class for `helm-project-list-buffers'.")

(defvar helm-project--lb-source nil
  "Source for `helm-project-list-buffers'.")

;;;###autoload
(defun helm-project-list-buffers (_arg)
  "List buffers in the current project using Helm."
  (interactive "P")
  (unless helm-project--lb-source
    (setq helm-project--lb-source (helm-make-source
                           "Project buffers" 'helm-project--lb-source-class)))
  (helm :sources '(helm-project--lb-source helm-source-buffer-not-found)
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width
        :prompt "Switch to buffer: "
        :buffer "*helm project list buffers*"))

;; * `helm-project-grep'

;;;###autoload
(defun helm-project-grep (arg)
  "Grep the current project using Helm."
  (interactive "P")
  (helm-aif (project-current)
      (helm-grep-ag (expand-file-name (project-root it)) arg)
    (user-error "Could not retrieve the current project")))

;; * `helm-project-switch-project'

;; ** :action

(defun helm-project--sp-remove-from-project-list (_project)
  "Remove all marked projects from the project list."
  (with-helm-buffer
    (mapc #'(lambda (project)
              (project--remove-from-project-list project nil))
          (helm-marked-candidates))
    (helm-force-update)))

(defmacro helm-project--sp-define-actions (&rest spec)
  "Define the actions for `helm-project-switch-project'.
Give each action a NAME and a CMD.  Unless SKIP-ACTION-DEF is
non-nil, the action is automatically defined.

\(fn (NAME CMD &key SKIP-ACTION-DEF)...)"
  (declare (debug (&rest (stringp symbolp &rest [":skip-action-def" booleanp]))))
  (let (defuns vals action)
    (while spec
      (cl-destructuring-bind (name cmd &key skip-action-def) (pop spec)
        (cl-check-type name stringp)
        (cl-check-type cmd symbolp)
        (cl-check-type skip-action-def booleanp)
        (setq action (intern (format "helm-project--sp-%s" cmd)))
        (unless skip-action-def
          (push `(defun ,action (project)
                   ,(format "Run `%s' in PROJECT." cmd)
                   (let ((default-directory project)
                         (project-current-inhibit-prompt t))
                     (call-interactively #',cmd)))
                defuns))
        (push `(cons ,name ',action) vals)))
    `(progn
       ,@(nreverse
          (cons `(defvar helm-project--sp-actions
                   (list ,@(nreverse vals))
                   "Actions for `helm-project-switch-project'.")
                defuns)))))

(helm-project--sp-define-actions
 ("Magit status" magit-status)
 ("VC-Dir" vc-dir)
 ("Dired" dired)
 ("Find files" helm-project-find-files)
 ("List buffers" helm-project-list-buffers)
 ("Browse project" helm-browse-project)
 ("Shell" project-shell)
 ("Eshell" project-eshell)
 ("Find regexp" project-find-regexp)
 ("Run `grep'" grep)
 ("Run `lgrep'" lgrep)
 ("Run `rgrep'" rgrep)
 ("Run `zrgrep'" zrgrep)
 ("Run `vc-git-grep'" vc-git-grep)
 ("Run `rg-project'" rg-project)
 ("Run `deadgrep'" deadgrep)
 ("Remove from project list" remove-from-project-list :skip-action-def t))

;; ** :action-transformer

(defun helm-project--sp-action-transformer (actions candidate)
  "Action transformer for `helm-project-switch-projects'.
If `magit-toplevel' for CANDIDATE returns nil, remove
`magit-status' from the available ACTIONS."
  (if (magit-toplevel candidate)
      actions
    (dolist (action '(magit-status) actions)
      (setq actions (rassq-delete-all action actions)))))

;; ** :persistent-action-if

(defun helm-project--sp-persistent-action-if (candidate)
  "Persistent action for `helm-project-switch-project'.
If `magit-toplevel' for CANDIDATE returns nil, run `vc-dir'.
Otherwise, run `magit-status'.  With a prefix argument, always
run `vc-dir'."
  (if (or current-prefix-arg (not (magit-toplevel candidate)))
      'vc-dir
    'magit-status))

;; ** :keymap

(defun helm-project--sp-run-remove-from-project-list ()
  "Run `helm-project--sp-remove-from-project-list' with a key binding."
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'helm-project--sp-remove-from-project-list
                   '(helm-project--sp-remove-from-project-list . never-split))
    (helm-execute-persistent-action 'helm-project--sp-remove-from-project-list)))
(put 'helm-project--sp-run-remove-from-project-list 'helm-only t)

(defmacro helm-project--sp-define-keymap (&rest spec)
  "Define the keymap for `helm-project-switch-project'.
Bind each KEY to a runner for its CMD.  Unless SKIP-RUNNER-DEF is
non-nil, the runner is defined automatically.

\(fn (KEY CMD &key SKIP-RUNNER-DEF)...)"
  (declare (debug (&rest (form symbolp &rest [":skip-runner-def" booleanp]))))
  (let ((temp-map (make-symbol "temp-map"))
        defuns props kbds action runner)
    (while spec
      (cl-destructuring-bind (key cmd &key skip-runner-def) (pop spec)
        (cl-check-type cmd symbolp)
        (cl-check-type skip-runner-def booleanp)
        (setq action (intern (format "helm-project--sp-%s" cmd))
              runner (intern (format "helm-project--sp-run-%s" cmd)))
        (unless skip-runner-def
          (push `(defun ,runner ()
                   ,(format "Run `%s' with a key binding." cmd)
                   (interactive)
                   (with-helm-alive-p
                     (helm-exit-and-execute-action ',action)))
                defuns)
          (push `(put ',runner 'helm-only t) props))
        (push `(define-key ,temp-map (kbd ,key) ',runner) kbds)))
    `(progn
       ,@(nreverse
          (cons `(defvar helm-project--sp-map
                   (let ((,temp-map (make-sparse-keymap)))
                     ,@(nreverse kbds)
                     ,temp-map)
                   "Keymap for `helm-project-switch-project'.")
                (nconc props defuns))))))

(helm-project--sp-define-keymap
 ("C-x g" magit-status)
 ("C-x v d" vc-dir)
 ("C-x d" dired)
 ("C-x C-f" helm-project-find-files)
 ("C-x b" helm-project-list-buffers)
 ("C-c C-b" helm-browse-project)
 ("C-c C-s" project-shell)
 ("C-c C-e" project-eshell)
 ("C-c C-f" project-find-regexp)
 ("C-c C-d" remove-from-project-list :skip-runner-def t)
 ("M-s g g" grep)
 ("M-s g l" lgrep)
 ("M-s g r" rgrep)
 ("M-s g z" zrgrep)
 ("M-s g v" vc-git-grep)
 ("M-s r" rg-project)
 ("M-s d" deadgrep))

;; ** Main function with associated data

(defclass helm-project--sp-source-class (helm-source-sync)
  ((init
    :initform (lambda ()
                (project--ensure-read-project-list)
                (helm-set-attr 'candidates project--list)))
   (action
    :initform 'helm-project--sp-actions)
   (action-transformer
    :initform 'helm-project--sp-action-transformer)
   (persistent-action-if
    :initform 'helm-project--sp-persistent-action-if)
   (persistent-help
    :initform "Magit if possible, else (C-u to force) VC-Dir")
   (keymap
    :initform helm-project--sp-map)
   (volatile
    :initform t)
   (migemo
    :initform t)
   (group
    :initform 'helm-project))
  :documentation "Source class for `helm-project-switch-project'.")

(defvar helm-project--sp-source nil
  "Source for `helm-project-switch-project'.")

;;;###autoload
(defun helm-project-switch-project ()
  "Switch project using Helm."
  (interactive)
  (unless helm-project--sp-source
    (setq helm-project--sp-source (helm-make-source
                           "Switch project" 'helm-project--sp-source-class)))
  (helm :sources 'helm-project--sp-source
        :prompt "Switch to project: "
        :buffer "*helm project switch project*"))

;; * Postamble

(provide 'helm-project)

;; Local Variables:
;; generated-autoload-file: "helm-project-loaddefs.el"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; time-stamp-start: "^;; Version: "
;; time-stamp-time-zone: t
;; time-stamp-format: "%Y%02m%02d%02H%02M%02S%"
;; time-stamp-end: "$"
;; End:

;;; helm-project.el ends here
