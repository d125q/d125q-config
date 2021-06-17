;;; helm-outline.el --- Jump to outline headings using Helm   -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210601141640
;; Keywords: convenience

;;; Commentary:

;; Provides `helm-outline' to jump to an outline heading from the
;; current buffer.

;;; Code:

;; * Preamble

(eval-when-compile
  (require 'cl-macs))
(require 'helm)
(require 'helm-utils)
(require 'outline)

;; * Options

(defgroup helm-outline nil
  "Jump to outline headings using Helm."
  :group 'helm)

(defcustom helm-outline-component-separator " / "
  "Separator for heading components."
  :group 'helm-outline
  :type 'string)

;; * Main program

(defun helm-outline--get-headings ()
  "Get headings from buffer.
Result is a list of (LEVEL NAME POSITION)."
  (let (result)
    (save-restriction
      (outline-map-region
       (lambda ()
         (push (helm-outline--process-heading) result))
       (point-min) (point-max)))
    (nreverse result)))

(defun helm-outline--process-heading ()
  "Process heading at point.
Result is (LEVEL NAME POSITION)."
  (list (funcall outline-level)
        (save-match-data
	  (save-excursion
	    (goto-char (match-end 0))
	    (string-trim (buffer-substring-no-properties
	                  (point) (progn
		                    (outline-end-of-heading)
		                    (point))))))
        (match-beginning 0)))

(defun helm-outline--process-headings (headings)
  "Process HEADINGS.
HEADINGS is a list of (LEVEL NAME POSITION).  Result a list
of (COMPONENTS POSITION), where COMPONENTS is a list of (NAME
PARENT-N-NAME ... PARENT-1-NAME)."
  ;; this does O(n^2) work at the moment; converting it to O(n) should
  ;; theoretically be possible but is tough because the heading levels
  ;; do not necessarily increase by one
  (cl-loop
   with temp
   for (level name position) in headings
   for prefix = (cl-loop
	         with prevs = temp
	         for target-level from (1- level) downto 1
	         for entry = (cl-loop
			      while prevs
			      for (prev-level prev-name) = (pop prevs)
			      when (= prev-level target-level)
			      return prev-name)
	         when entry collect entry)
   do (push (list level name) temp)
   collect (list (cons name prefix) position)))

(defun helm-outline--candidates ()
  "Compute candidates for `helm-outline'."
  (with-helm-current-buffer
    (cl-loop
     for (components position) in (helm-outline--process-headings
                                   (helm-outline--get-headings))
     for formatted-path = (mapconcat #'identity
                                     components
                                     helm-outline-component-separator)
     collect (cons formatted-path position))))

(defun helm-outline--action (candidate)
  "Jump to and open CANDIDATE."
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (goto-char candidate)
  (outline-show-entry)
  (helm-highlight-current-line))

(defun helm-outline--persistent-action (candidate)
  "Close previous invisible position and preview CANDIDATE."
  (switch-to-buffer helm-current-buffer)
  (helm-aif (helm-get-attr 'prev-invisible-position)
      (save-excursion
        (goto-char it)
        (outline-hide-entry)
        (helm-set-attr 'prev-invisible-position nil)))
  (goto-char candidate)
  (when (outline-invisible-p (save-excursion
                               (outline-end-of-heading)
                               (point)))
    (outline-show-entry)
    (helm-set-attr 'prev-invisible-position (point)))
  (outline-show-entry)
  (helm-highlight-current-line))

(defun helm-outline--cleanup ()
  "Close previous invisible position before quitting."
  (switch-to-buffer helm-current-buffer)
  (helm-aif (helm-get-attr 'prev-invisible-position)
      (save-excursion
        (goto-char it)
        (outline-hide-entry)
        (helm-set-attr 'prev-invisible-position nil))))

(defclass helm-outline--source-class (helm-source-sync)
  ((prev-invisible-position :initarg :prev-invisible-position
                            :initform nil
                            :custom integer
                            :documentation "The previous invisible position.")
   (init :initform (lambda ()
                     (helm-set-attr 'prev-invisible-position nil)))
   (candidates :initform 'helm-outline--candidates)
   (action :initform 'helm-outline--action)
   (persistent-action :initform 'helm-outline--persistent-action)
   (persistent-help :initform "Jump to heading")
   (cleanup :initform 'helm-outline--cleanup)
   (nomark :initform t)
   (must-match :initform t)
   (group :initform 'helm-outline))
  :documentation "Source class for `helm-outline'.")

(defvar helm-outline--source nil
  "Source for `helm-outline'.")

;;;###autoload
(defun helm-outline ()
  "Jump to outline heading using Helm."
  (interactive)
  (unless helm-outline--source
    (setq helm-outline--source (helm-make-source
                                   "Jump to outline heading"
                                   'helm-outline--source-class)))
  (helm :sources 'helm-outline--source
        :prompt "Jump to heading: "
        :buffer "*helm outline*"))

;; * Postamble

(provide 'helm-outline)

;; Local Variables:
;; generated-autoload-file: "helm-outline-loaddefs.el"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; time-stamp-start: "^;; Version: "
;; time-stamp-time-zone: t
;; time-stamp-format: "%Y%02m%02d%02H%02M%02S%"
;; time-stamp-end: "$"
;; End:

;;; helm-outline.el ends here
