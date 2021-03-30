;;; d125q-utils.el --- d125q's utils for GNU Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; Version: 20210330T160158+0200
;; Keywords: convenience

;;; Commentary:

;; These are utilities for GNU Emacs designed to make various tasks
;; easier.  Special focus is placed on making it more convenient to
;; configure things such as key bindings.

;;; Code:

;; * Preamble

(require 'cl-lib)
(require 'epa)

;; * Metaprogramming and performing general-purpose tasks

(defun as-keyword (object)
  "Convert OBJECT to keyword.

\(as-keyword \\='symbol)
    => :symbol
\(as-keyword \"string\")
    => :string
\(as-keyword :keyword)
    => :keyword"
  (if (keywordp object)
      object
    (intern (format ":%s" object))))

(defun mapsyms (fun sexpr)
  "Map FUN over the symbols of SEXPR.

\(mapsyms (lambda (sym) (intern (format \"namespace-%s\" sym)))
         \\='(setq var1 val1
                var2 val2))
    => (namespace-setq namespace-var1 namespace-val1
                       namespace-var2 namespace-val2)"
  (cond
   ((and (symbolp sexpr) sexpr) (funcall fun sexpr))
   ((atom sexpr) sexpr)
   (t (cons (mapsyms fun (car sexpr)) (mapsyms fun (cdr sexpr))))))

(defun make-string-abbreviator (strings)
  "Make an abbreviator for STRINGS.

\(let* ((strings \\='(\"aye\" \"captain\" \"snow\" \"sled\"))
       (abbreviator (make-string-abbreviator strings)))
  (mapcar (lambda (string) (gethash string abbreviator)) strings))
    => (\"a\" \"c\" \"sn\" \"sl\")"
  (cl-assert (cl-every #'stringp strings))
  (setq strings (seq-uniq strings #'equal))
  (cl-loop
   with maxlen = (apply #'max (mapcar #'seq-length strings))
   and string->prefix = (make-hash-table :test #'equal)
   and prefix->strings = (make-hash-table :test #'equal)
   for len from 1 to maxlen
   do (cl-loop
       for string in strings
       unless (gethash string string->prefix)
       do (condition-case nil
              (let* ((prefix (seq-subseq string 0 len))
                     (strings (cons string (gethash prefix prefix->strings))))
                (puthash prefix strings prefix->strings))
            ;; in this case, `string' is a proper substring of some
            ;; other member of `strings', so we consider it to
            ;; "abbreviate" to itself
            (error (puthash string string string->prefix))))
   do (cl-loop
       ;; if the prefix maps to only one element, it's good to go
       for prefix being the hash-keys of prefix->strings
       using (hash-values strings)
       unless (cdr strings)
       do (puthash (car strings) prefix string->prefix))
   ;; prepare for the next iteration
   do (clrhash prefix->strings)
   finally return string->prefix))

(defun decrypt-file (file)
  "Decrypt the contents of FILE.

\(defvar secrets (read
                 (decrypt-file
                  (locate-user-emacs-file \"secrets.gpg\")))
    => The GPG-encrypted file will be decrypted and read as a
       Lisp expression whose value will be stored in `secrets'."
  (setq file (expand-file-name file))
  (let ((context (epg-make-context epa-protocol)))
    (epg-context-set-passphrase-callback
     context
     #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback
     context
     (cons #'epa-progress-callback-function
           (format "Decrypting %s…" (file-name-nondirectory file))))
    (condition-case error
        (epg-decrypt-file context file nil)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))))

(defun wmctrl-raise-frame (&optional frame)
  "Raise FRAME using `wmctrl'.

If FRAME is nil, the selected frame is used."
  (setq frame (or frame (selected-frame)))
  (call-process
   "wmctrl" nil nil nil "-i" "-R"
   (frame-parameter frame 'outer-window-id)))

(defmacro setq-nreverse (&rest vars)
  "Reverse VARS in-place.

\(let ((list (number-sequence 1 5)))
  (setq-nreverse list)
  list)
    => (5 4 3 2 1)"
  `(setq ,@(mapcan (lambda (var) `(,var (nreverse ,var))) vars)))

(defmacro with-namespace (namespace &rest body)
  "Format BODY with NAMESPACE.

\(with-namespace icomplete
  (setq %s-prospects-height 2
        %s-show-matches-on-no-input t))
    => (setq icomplete-prospects-height 2
             icomplete-show-matches-on-no-input t)"
  (declare (indent 1))
  (macroexp-progn
   (mapsyms (lambda (sym)
              (intern (format (symbol-name sym) namespace)))
            body)))

(defmacro with-gensyms (syms &rest body)
  "Bind SYMS to fresh gensyms and evaluate BODY.

\(with-gensyms (funname)
  (fset funname (lambda (arg)
                  (format \"Hello, %s!\" arg)))
  (funcall funname \"World\"))
    => \"Hello, World!\""
  (declare (indent 1))
  (cl-assert (cl-every #'symbolp syms))
  `(let ,(cl-mapcar #'list syms '#1=((gensym "--") . #1#))
     ,@body))

(cl-defmacro with-eval-after-package (pkg ext-p vars fns &rest body)
  "Execute BODY after PKG has been loaded.

- If PKG is nil, BODY will be executed immediately.

- If EXT-P is non-nil, PKG will be assumed to be an external
  package.

- VARS is a list of variables used within BODY and assumed to be
  defined by PKG.

- FUNS is a list of functions used within BODY and assumed to be
  defined by PKG.

See also `with-eval-after-load'.

\(with-eval-after-package magit-extras t nil (ido-enter-magit-status)
  (define-key ido-common-completion-map
    (kbd \"C-x g\") \\='ido-enter-magit-status))
    => (with-eval-after-load \\='magit-extras
         (declare-function ido-enter-magit-status \"ext:magit-extras\")
         (define-key ido-common-completion-map
           (kbd \"C-x g\") \\='ido-enter-magit-status)))"
  (declare (indent 4))
  (let ((defvar-exprs (mapcar (lambda (var) `(defvar ,var)) vars)))
    (if-let ((file (when pkg (format (if ext-p "ext:%s" "%s") pkg))))
        `(with-eval-after-load ',pkg
           ,@(nconc defvar-exprs
                    (mapcar (lambda (fn) `(declare-function ,fn ,file)) fns)
                    body))
      (macroexp-progn
       (nconc defvar-exprs body)))))

(defmacro deflambda (args &rest body)
  "Define a lambda with BODY and bindings given by ARGS.

\(funcall (deflambda (&key k1 k2)
           (format \"k1 = %s, k2 = %s\" k1 k2))
         :k1 \"foo\" :k2 \"bar\")
    => \"k1 = foo, k2 = bar\""
  (declare (indent 1))
  (with-gensyms (expr)
    `(lambda (&rest ,expr) (cl-destructuring-bind ,args ,expr ,@body))))

(defmacro define-controls (sym)
  "Define controls to turn on, turn off, or toggle SYM.

\(define-controls indent-tabs-mode)
    => (progn
         (defun turn-on-indent-tabs-mode ()
           \"Turn on \\=`indent-tabs-mode\\='.\"
           (interactive)
           (setq indent-tabs-mode t))
         (defun turn-off-indent-tabs-mode ()
           \"Turn off \\=`indent-tabs-mode\\='.\"
           (interactive)
           (setq indent-tabs-mode nil))
         (defun toggle-indent-tabs-mode ()
           \"Toggle \\=`indent-tabs-mode\\='.\"
           (interactive)
           (setq indent-tabs-mode (not indent-tabs-mode))))"
  (cl-assert (symbolp sym))
  (let* ((turn-on (intern (format "turn-on-%s" sym)))
         (turn-off (intern (format "turn-off-%s" sym)))
         (toggle (intern (format "toggle-%s" sym))))
    `(progn
       (defun ,turn-on ()
         ,(format "Turn `%s' on." sym)
         (interactive)
         (setq ,sym t))
       (defun ,turn-off ()
         ,(format "Turn `%s' off." sym)
         (interactive)
         (setq ,sym nil))
       (defun ,toggle ()
         ,(format "Toggle `%s'." sym)
         (interactive)
         (setq ,sym (not ,sym))))))

(defmacro define-dispatcher (name main-fn &rest other-fns)
  "Define NAME to dispatch to MAIN-FN and OTHER-FNS."
  `(cl-defun ,name (arg &aux (current-prefix-arg nil) (tgt (car-safe arg)))
     ,(format "Dispatch to `%s' and %d other(s)." main-fn (length other-fns))
     (interactive "P")
     (if tgt
         (cl-loop
          with val = 1
          while (< val tgt)
          for other-fn in ',other-fns
          do (setq val (ash val 2))
          finally return (if (= val tgt)
                             (funcall-interactively other-fn)
                           (user-error "Could not dispatch until %d" tgt)))
       (call-interactively ',main-fn))))

;; * Settings and customizations

(cl-defmacro set-variables ((&key pkg ext-p &aux vars assgns) &rest spec)
  "Set variables according to SPEC.

- SPEC is a list of (VAR VAL) such that each VAR will be set to
  its corresponding VAL.

- PKG is a symbol that is assumed to define each VAR, and the
  settings will take effect only after it has been loaded.  If it
  is nil, the settings will take effect immediately.

- If EXT-P is non-nil, PKG is assumed to be an external package."
  (declare (indent 1))
  (while spec
    (let ((var (pop spec))
          (val (pop spec)))
      (push var vars)
      (push `(,var ,val) assgns)))
  `(with-eval-after-package ,pkg ,ext-p ,(nreverse vars) nil
     (setq ,@(apply #'nconc (nreverse assgns)))))

(defmacro customize-variables (&rest spec)
  "Customize variables according to SPEC.

- SPEC is a list of (VAR VAL) such that each VAR will be
  customized to its corresponding VAL."
  (declare (indent 0))
  `(custom-set-variables
    ,@(cl-loop for (var expr) on spec by 'cddr
               collect `'(,var ,expr))))

(defmacro customize-faces (&rest spec)
  "Customize faces according to SPEC.

- SPEC is a list of (DISP . DEFS), where DISP specifies the
  condition for which DEFS should take effect.

- Each DEFS is a list of (FACE . ATTRS), where ATTRS specifies
  the attributes of FACE.

For details, see `custom-set-faces' and `defface'.

\(customize-faces
 (((background light))
  (face-1 (:foreground light-fg))
  (face-2 (:foreground light-fg))
  (face-3 (:foreground light-fg)))
 (((background dark))
  (face-1 (:foreground dark-fg))
  (face-2 (:foreground dark-fg))
  (face-3 (:foreground dark-fg))))
    => The three faces will have their foreground set to either
       `light-fg' or `dark-fg' depending on whether Emacs is using
       a light or a dark background."
  `(custom-set-faces
    ,@(cl-loop
       with ht = (make-hash-table :test #'eq)
       for (disp . defs) in spec
       do (cl-loop
           for (face . attrs) in defs
           for upd = (cons (cons disp attrs) (gethash face ht))
           do (puthash face upd ht))
       finally return (cl-loop
                       for face being the hash-keys of ht
                       using (hash-values def)
                       collect `'(,face ,def)))))

(cl-defmacro plstore/set-variables ((plstore name &key pkg ext-p) &rest vars)
  "Using the PLSTORE entry with NAME, set VARS.

- VARS is a list of VAR.

- PKG is a symbol that is assumed to define each VAR, and the
  settings will take effect only after it has been loaded.  If it
  is nil, the settings will take effect immediately.

- If EXT-P is non-nil, PKG is assumed to be an external package.

\(plist/set-variables (plstore erc :pkg erc) erc-password)
    => Once `erc' has been loaded, `erc-password' will be set
       from (plstore-get plstore \"erc\")."
  (declare (indent 1))
  `(set-variables (:pkg ,pkg :ext-p ,ext-p)
     ,@(cl-loop
        with entry = `(plstore-get ,plstore ,(format "%s" name))
        with plist = `(cdr ,entry)
        for var in vars
        for val = `(plist-get ,plist ,(as-keyword var))
        nconc `(,var ,val))))

(cl-defmacro plstore/customize-variables ((plstore name) &rest vars)
  "Using the PLSTORE entry with NAME, customize VARS."
  (declare (indent 1))
  `(customize-variables
     ,@(cl-loop
        with entry = `(plstore-get ,plstore ,name)
        with plist = `(cdr ,entry)
        for var in vars
        for val = `(plist-get ,plist ,(as-keyword var))
        nconc `(,var ,val))))

(cl-defmacro plist/set-variables ((plist name &key pkg ext-p) &rest vars)
  "Using the PLIST entry with NAME, set VARS.

- VARS is a list of VAR.

- PKG is a symbol that is assumed to define each VAR, and the
  settings will take effect only after it has been loaded.  If it
  is nil, the settings will take effect immediately.

- If EXT-P is non-nil, PKG is assumed to be an external package.

\(plist/set-variables (plist erc :pkg erc) erc-password)
    => Once `erc' has been loaded, `erc-password' will be set
       from (plist-get plist :erc)."
  (declare (indent 1))
  `(set-variables (:pkg ,pkg :ext-p ,ext-p)
     ,@(cl-loop
        with entry = `(plist-get ,plist ,(as-keyword name))
        for var in vars
        for val = `(plist-get ,entry ,(as-keyword var))
        nconc `(,var ,val))))

(cl-defmacro plist/customize-variables ((plist name) &rest vars)
  "Using the PLIST entry with NAME, customize VARS.

\(plist/customize-variables (plist gnus)
  gnus-select-method
  gnus-secondary-select-methods)
    => `gnus-select-method' and `gnus-secondary-select-methods'
       will be customized from (plist-get plist :gnus)."
  (declare (indent 1))
  `(customize-variables
     ,@(cl-loop
        with entry = `(plist-get ,plist ,(as-keyword name))
        for var in vars
        for val = `(plist-get ,entry ,(as-keyword var))
        nconc `(,var ,val))))

;; * Key bindings and transient keymaps

(defun parse-key (key)
  "Parse KEY to its internal representation.

KEY must satisfy `stringp' or `vectorp'.  Strings are parsed with
`kbd' and vectors are returned as-is.

(equal (parse-key \"C-x g\") (kbd \"C-x g\"))
    => t
(equal (parse-key [remap find-file]) [remap find-file])
    => t"
  (cond
   ((stringp key) (kbd key))
   ((vectorp key) key)
   (t (user-error "KEY must be string or vector: %s") key)))

(cl-defun bind-key (map key cmd &aux (parsed-key (parse-key key)))
  "Parse KEY as PARSED-KEY and bind it to CMD in MAP.

If MAP is nil, the key binding will be made global."
  (if map
      (define-key map parsed-key cmd)
    (global-set-key parsed-key cmd)))

(cl-defmacro define-key-bindings ((&key map prefix prepend pkg ext-p) &rest spec)
  "Define key bindings with SPEC.

- SPEC is a list of (KEY CMD &key NODECL) such that each CMD will
  be bound to its corresponding KEY.  If NODECL is non-nil, then
  CMD will not be declared as being defined by PKG.

- MAP is a keymap that will contain the key bindings.  If it is
  nil, the key bindings will be made global.

- PREFIX is a key that will act as a prefix for each key binding.
  It will be established through a prefix keymap bound to a
  gensym.

- PREPEND is a key that will be prepended to each KEY.

- PKG is a symbol that is assumed to define each CMD, and the key
  bindings will be defined only after it has been loaded.  If it
  is nil, the key bindings will be defined immediately.

- If EXT-P is non-nil, PKG is assumed to be an external package.

\(define-key-bindings (:map pyvenv-mode-map :prefix \"C-c v\"
                           :pkg pyvenv :ext-p t)
  (\"a\" pyvenv-activate)
  (\"w\" pyvenv-workon)
  (\"d\" pyvenv-deactivate)
  (\"c\" pyvenv-create))
    => Once `pyvenv' gets loaded (e.g., by running `pyvenv-mode'),
       create the specified key bindings under the C-c v prefix
       and place them in `pyvenv-mode-map'."
  (declare (indent 1))
  (let ((vars (when map (list map)))
        fns
        (init-prefix-exprs
         (when prefix
           (with-gensyms (prefix-cmd)
             (prog1
                 `((defvar ,prefix-cmd)
                   (define-prefix-command ',prefix-cmd)
                   (bind-key ,map ,prefix ,prefix-cmd))
               (setq map prefix-cmd)))))  ; use `prefix-cmd' as the keymap
        bind-key-exprs)
    (while spec
      (cl-destructuring-bind (key cmd &key nodecl) (pop spec)
        (unless nodecl
          (push cmd fns))
        (when prepend
          (setq key (cond
                     ((and (vectorp prepend) (vectorp key))
                      (vconcat prepend [ ] key))
                     ((and (stringp prepend) (stringp key))
                      (concat prepend " " key))
                     (t
                      (user-error
                       (mapconcat
                        #'identity
                        '("PREPEND and KEY must both be vectors or strings"
                          "PREPEND = %s" "KEY = %s")
                        "\n")
                       prepend key)))))
        (push `(bind-key ,map ,key ',cmd) bind-key-exprs)))
    (setq-nreverse fns bind-key-exprs)
    `(with-eval-after-package ,pkg ,ext-p ,vars ,fns
       ,@(nconc init-prefix-exprs bind-key-exprs))))

(defun deactivate-transient-map ()
  "Do nothing but deactivate the current transient map."
  (interactive))

(cl-defmacro define-transient-map ((&key map
                                         pkg ext-p
                                         exit-key
                                         persist-by-default)
                                   name activation-key &rest spec)
  "Define a transient keymap with NAME, ACTIVATION-KEY, and SPEC.

- ACTIVATION-KEY will activate the transient keymap.  If MAP is
  non-nil, the binding of ACTIVATION-KEY will be placed in MAP.
  Otherwise, it will be made global.

- SPEC is a list of (KEY CMD &key PERSIST), such that each CMD
  will be bound to its corresponding KEY.  If PERSIST is non-nil,
  the binding will be persistent.  Otherwise, the persistence of
  the binding will be determined by PERSIST-BY-DEFAULT.

- If EXIT-KEY is non-nil, it will exit the transient keymap.

- If PKG is non-nil, it will be assumed to define each CMD and
  its loading will cause the transient keymap to be defined.
  Otherwise, the transient keymap will be defined immediately.

- If EXT-P is non-nil, PKG will be assumed to be an external
  package.

\(define-transient-map (:map flymake-mode-map :pkg flymake) \"C-c !\"
  (\"n\" flymake-goto-next-error :persist t)
  (\"p\" flymake-goto-prev-error :persist t)
  (\"s\" flymake-start)
  (\"l\" flymake-switch-to-log-buffer)
  (\"d\" flymake-show-diagnostics-buffer)
  (\"r\" flymake-running-backends)
  (\"D\" flymake-disabled-backends)
  (\"R\" flymake-reporting-backends))
    => Once `flymake' has been loaded (e.g., by running `flymake-mode'),
       define a transient keymap that contains the specified key bindings
       and is activated by C-c !."
  (declare (indent 2))
  (with-gensyms (temp-map)
    (let ((transient-map (intern (format "%s-transient-map" name)))
          (keep-pred (intern (format "%s-keep-pred" name)))
          (activation-cmd (intern (format "activate-%s-transient-map" name)))
          (vars (when map
                  (list map)))
          cmds
          (bind-key-exprs (when exit-key
                            (list `(bind-key ,temp-map ,exit-key
                                             'deactivate-transient-map))))
        persistent-cmds)
      (while spec
        (cl-destructuring-bind
            (key cmd &key (persist nil persist-supplied-p))
            (pop spec)
          (push cmd cmds)
          (push `(bind-key ,temp-map ,key ',cmd) bind-key-exprs)
          (when (or (and persist-supplied-p persist)
                    (and (not persist-supplied-p) persist-by-default))
            (push cmd persistent-cmds))))
      (setq-nreverse cmds
                     bind-key-exprs
                     persistent-cmds)
      `(with-eval-after-package ,pkg ,ext-p ,vars ,cmds
         (defvar ,transient-map
           (let ((,temp-map (make-sparse-keymap)))
             ,@bind-key-exprs
             ,temp-map))
         (defun ,keep-pred ()
           (and (not (eq this-command 'deactivate-transient-map))
                (memq this-command ',persistent-cmds)))
         (defun ,activation-cmd ()
           (interactive)
           (set-transient-map ,transient-map ',keep-pred))
         (bind-key ,map ,activation-key ',activation-cmd)))))

;; * Postamble

(provide 'd125q-utils)

;;; Local Variables:
;;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;;; time-stamp-start: "^;; Version: "
;;; time-stamp-format: "%Y%02m%02dT%02H%02M%02S%5z"
;;; time-stamp-end: "$"
;;; End:

;;; d125q-utils.el ends here
