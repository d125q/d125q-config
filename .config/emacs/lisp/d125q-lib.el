;;; d125q-lib.el --- d125q's extensions for GNU Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210408112848
;; Keywords: convenience

;;; Commentary:

;; These are extensions to Emacs Lisp designed to make it easier to
;; customize settings, define key bindings, etc.  This file contains
;; the portions of the code which should always be present.

;;; Code:

(require 'seq)
(require 'epa)
(eval-when-compile
  (require 'cl-macs)
  (require 'eieio-core))                ; for the `list-of' type

(cl-deftype seq-of (elem-type)
  `(and seqp
        (satisfies (lambda (seq)
                     (cl-every (lambda (elem)
                                 (cl-typep elem ',elem-type))
                               seq)))))

(defun d125q-keyp (object)
  "Return t if OBJECT is a key; i.e., a string or a vector."
  (declare (pure t) (side-effect-free t))
  (or (stringp object) (vectorp object)))

(defun d125q-dotted-pair-p (object)
  "Return t if OBJECT is a dotted pair."
  (and (cdr object) (atom (cdr object))))

(defun d125q-object-to-keyword (object)
  "Convert OBJECT to a keyword."
  (declare (pure t) (side-effect-free t))
  (if (keywordp object)
      object
    (intern (format ":%s" object))))

(defun d125q-mapsyms (fun sexp)
  "Map FUN over the symbols of SEXP."
  (cl-check-type fun functionp)
  (cond
   ((and (symbolp sexp) sexp) (funcall fun sexp))
   ((atom sexp) sexp)
   (t (cons (d125q-mapsyms fun (car sexp))
            (d125q-mapsyms fun (cdr sexp))))))

(defun d125q-compute-shortest-unique-prefixes (strs)
  "Compute the shortest unique prefixes of STRS.
Return a hash table mapping each member of STRS to the shortest
prefix not shared by any other member of STRS."
  (declare (pure t)
           (side-effect-free t))
  (cl-check-type strs (seq-of stringp))
  (setq strs (seq-uniq strs #'equal))
  (cl-loop
   with maxlen = (apply #'max (mapcar #'length strs))
   and str->prefix = (make-hash-table :test #'equal)
   and prefix->strs = (make-hash-table :test #'equal)
   for len from 1 to maxlen
   do (cl-loop
       for str in strs
       unless (gethash str str->prefix)
       do (condition-case nil
              (let* ((prefix (substring str 0 len))
                     (strs (cons str (gethash prefix prefix->strs))))
                (puthash prefix strs prefix->strs))
            ;; in this case, `str' is a proper substring of some other
            ;; member of `strs'; consider it to abbreviate to itself
            (error (puthash str str str->prefix))))
   do (cl-loop
       ;; if the prefix maps to only one element, it's good to go
       for prefix being the hash-keys of prefix->strs
       using (hash-values strs)
       unless (cdr strs)
       do (puthash (car strs) prefix str->prefix))
   ;; prepare for the next iteration
   do (clrhash prefix->strs)
   finally return str->prefix))

(defun d125q-decrypt-file-into-string (file)
  "Decrypt FILE and return the result as a string."
  (setq file (expand-file-name file))
  (let ((context (epg-make-context epa-protocol))
        (passphrase-callback #'epa-passphrase-callback-function)
        (progress-callback (format "Decrypting %s"
                                   (file-name-nondirectory file))))
    (epg-context-set-passphrase-callback
     context passphrase-callback)
    (epg-context-set-progress-callback
     context (cons #'epa-progress-callback-function progress-callback))
    (condition-case error
        (epg-decrypt-file context file nil)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))))

(defun d125q-wmctrl-raise-frame (&optional frame)
  "Raise FRAME using `wmctrl'.
If FRAME is nil, use the selected frame."
  (call-process
   "wmctrl" nil nil nil "-i" "-R"
   (frame-parameter frame 'outer-window-id)))

(defun d125q-bind-key (map key cmd)
  "In MAP, bind KEY to CMD.
If MAP is nil, make the binding global."
  (let ((parsed-key (cl-etypecase key
                      (stringp (kbd key))
                      (vectorp key))))
    (if map
        (define-key map parsed-key cmd)
      (global-set-key parsed-key cmd))))

(provide 'd125q-lib)
(unless (load "d125q-macs-loaddefs" 'noerror 'nomessage)
  (require 'd125q-macs))

;; Local Variables:
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; time-stamp-start: "^;; Version: "
;; time-stamp-time-zone: t
;; time-stamp-format: "%Y%02m%02d%02H%02M%02S%"
;; time-stamp-end: "$"
;; End:

;;; d125q-lib.el ends here
