.PHONY: test last

test:
	emacs -f package-initialize -batch --eval "(setq load-path (cons \".\" load-path))" -f buttercup-run-discover

last:
	emacs -f package-initialize -batch -l run-utils.el  --eval "(run-last-test)"

single:
	emacs -f package-initialize -batch -l run-utils.el  --eval "(run-test ${ARG})"

copy-template:
	emacs --script templates/copy-template.el ${ARG} templates
