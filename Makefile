PACKAGE_BASENAME := emake
.DEFAULT_GOAL: help

clean::
	cp emake.el emake.el.bak # standard Makefile kills emake.el; save it off.
include emake.mk
clean::
	mv emake.el.bak emake.el # ...and restore.

test: lint-checkdoc lint-package-lint ## run various linting tools
