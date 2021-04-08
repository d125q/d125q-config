;;; d125q-macs.el --- d125q's macros for GNU Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210408112851
;; Keywords: convenience

;;; Commentary:

;; These are extensions to Emacs Lisp designed to make it easier to
;; customize settings, define key bindings, etc.  This file contains
;; the portions of the code which should be autoloaded, but need only
;; be present if the compiler or interpreter is used.  In other words,
;; this file is not necessary for executing compiled code.

;;; Code:

(require 'cl-lib)
(require 'd125q-lib)

;;;###autoload
(defmacro d125q-lambda (&rest form)
  "Return an anonymous function.
Like normal `lambda', except ARGLIST allows full Common Lisp
conventions.  See the Info node `(cl)Argument Lists' for more
details.

\(fn ARGLIST [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 2)
           (indent defun)
           (debug (&define cl-lambda-list
                           cl-declarations-or-string
                           [&optional ("interactive" interactive)]
                           def-body)))
  `(function (lambda . ,(cl--transform-lambda form 'cl-none))))

;;;###autoload
(defmacro d125q-setq-nreverse (&rest syms)
  "Reverse SYMS in-place."
  (declare (debug (&rest symbolp)))
  `(setq
    ,@(mapcan (lambda (sym)
                (cl-check-type sym symbolp)
                `(,sym (nreverse ,sym)))
              syms)))

;;;###autoload
(defmacro d125q-with-gensyms (symbols &rest body)
  "Bind SYMBOLS to fresh gensyms and evaluate BODY."
  (declare (indent 1)
           (debug ((&rest symbolp) body)))
  `(let ,(cl-mapcar (lambda (sym val)
                      (cl-check-type sym symbolp)
                      `(,sym ,val))
                    symbols '#1=((gensym "d125q--") . #1#))
     ,@body))

;;;###autoload
(defmacro d125q-define-controls (sym)
  "Make controls to turn on, turn off, and toggle SYM."
  (declare (debug (symbolp)))
  (cl-check-type sym symbolp)
  (let ((turn-on (intern (format "turn-on-%s" sym)))
        (turn-off (intern (format "turn-off-%s" sym)))
        (toggle (intern (format "toggle-%s" sym))))
    `(progn
       (defun ,turn-on ()
         ,(format "Enable `%s'." sym)
         (interactive)
         (setq ,sym t))
       (defun ,turn-off ()
         ,(format "Disable `%s'." sym)
         (interactive)
         (setq ,sym nil))
       (defun ,toggle ()
         ,(format "Toggle `%s'." sym)
         (interactive)
         (setq ,sym (not ,sym))))))

;;;###autoload
(cl-defmacro d125q-customizeq (&rest spec &aux args sym exp)
  "For each SYM, store EXP as its saved value.

\(fn [SYM EXP]...)"
  (declare (debug (&rest [symbolp sexp])))
  (cl-assert (cl-evenp (length spec)))
  (while spec
    (setq sym (pop spec)
          exp (pop spec))
    (cl-check-type sym symbolp)
    (push `'(,sym ,exp) args))
  `(custom-set-variables ,@args))

;;;###autoload
(cl-defmacro d125q-plist-customizeq ((plist prop) &rest syms)
  "Using the PLIST entry with PROP, customize SYMS."
  (declare (indent 1)
           (debug ((sexp sexp) &rest symbolp)))
  `(d125q-customizeq
    ,@(cl-loop
       with entry = `(plist-get ,plist ,prop)
       for sym in syms
       do (cl-check-type sym symbolp)
       for exp = `(plist-get ,entry ,(d125q-object-to-keyword sym))
       nconc `(,sym ,exp))))

;;;###autoload
(cl-defmacro d125q-plist-setq ((plist prop) &rest syms)
  "Using the PLIST entry with PROP, set SYMS."
  (declare (indent 1)
           (debug ((form form) &rest symbolp)))
  (d125q-with-gensyms (entry-var)
    `(let ((,entry-var (plist-get ,plist ,prop)))
       (setq
        ,@(cl-loop
           for sym in syms
           do (cl-check-type sym symbolp)
           for exp = `(plist-get ,entry-var ,(d125q-object-to-keyword sym))
           nconc `(,sym ,exp))))))

;;;###autoload
(cl-defmacro d125q-bind-keys ((&key map prefix) &rest spec)
  "Bind each KEY to its CMD.

\(fn (&key MAP PREFIX) [KEY CMD]...)"
  (declare (indent 1)
           (debug ((&rest [&or [":map" form]
                               [":prefix" form]])
                   &rest [atom symbolp])))
  (cl-assert (cl-evenp (length spec)))
  (d125q-with-gensyms (map-var
                       prefix-var
                       prefix-cmd-var)
    (let ((init-prefix-expr
           `(when ,prefix-var
              (defvar ,prefix-cmd-var)
              (define-prefix-command ',prefix-cmd-var)
              (d125q-bind-key ,map-var ,prefix-var ,prefix-cmd-var)
              (setq ,map-var ,prefix-cmd-var)))
          (bind-key-exprs
           (cl-loop for (key cmd) on spec by 'cddr
                    do (cl-check-type key atom)
                    do (cl-check-type cmd symbolp)
                    collect `(d125q-bind-key ,map-var ,key ',cmd))))
      `(let ((,map-var ,map)
             (,prefix-var ,prefix))
         ,init-prefix-expr
         ,@bind-key-exprs))))

;;;###autoload
(cl-defmacro d125q-define-transient-map ((sym activation-key
                                              &key map persist-by-default)
                                         &rest spec)
  "Define a transient keymap with SYM and ACTIVATION-KEY.
In it, bind each KEY to its CMD.

\(fn (NAME ACTIVATION-KEY &key MAP PERSIST-BY-DEFAULT) [KEY CMD &key PERSIST]...)"
  (declare (indent 1)
           (debug ((symbolp form &rest [&or [":map" form]
                                            [":persist-by-default" atom]])
                   &rest [atom symbolp])))
  (cl-check-type sym symbolp)
  (cl-check-type persist-by-default atom)
  (d125q-with-gensyms (temp-map-var)
    (let* ((transient-map (intern (format "%s-transient-map" sym)))
           (keep-pred (intern (format "%s-keep-pred" sym)))
           (activation-cmd (intern (format "activate-%s" transient-map)))
           bind-key-exprs
           persistent-cmds)
      (while spec
        (cl-destructuring-bind
            (key cmd &key (persist persist-by-default)) (pop spec)
          (cl-check-type key atom)
          (cl-check-type cmd symbolp)
          (push `(d125q-bind-key ,temp-map-var ,key ',cmd) bind-key-exprs)
          (when persist
            (push cmd persistent-cmds))))
      `(progn
         (defvar ,transient-map
           (let ((,temp-map-var (make-sparse-keymap)))
             ,@(nreverse bind-key-exprs)
             ,temp-map-var)
           ,(format "Transient map for `%s'." sym))
         (defun ,keep-pred ()
           ,(format "Decide if `%s' should remain active." transient-map)
           (memq this-command ',(nreverse persistent-cmds)))
         (defun ,activation-cmd ()
           ,(format "Activate `%s'." transient-map)
           (interactive)
           (set-transient-map ,transient-map ',keep-pred))
         (d125q-bind-key ,map ,activation-key ',activation-cmd)))))

(provide 'd125q-macs)

;; Local Variables:
;; generated-autoload-file: "d125q-macs-loaddefs.el"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; time-stamp-start: "^;; Version: "
;; time-stamp-time-zone: t
;; time-stamp-format: "%Y%02m%02d%02H%02M%02S%"
;; time-stamp-end: "$"
;; End:

;;; d125q-macs.el ends here
