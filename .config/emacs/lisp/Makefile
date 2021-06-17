EMACS = emacs
EMACSOPT = --batch --no-site-file --no-site-lisp

# The actual Emacs command run in the targets below.
# Prevent any setting of EMACSLOADPATH in user environment causing problems.
emacs = EMACSLOADPATH= '$(EMACS)' $(EMACSOPT)

.PHONY: autoloads
autoloads: d125q-macs.el d125q-lib.el helm-project.el helm-outline.el
	$(emacs) -l autoload \
	--eval '(setq autoload-ensure-writable t)' \
	--eval '(setq generated-autoload-file "d125q-loaddefs.el")' \
	-f batch-update-autoloads .

.PHONY: clean
clean:
	-rm d125q-loaddefs.el d125q-macs-loaddefs.el helm-project-loaddefs.el
