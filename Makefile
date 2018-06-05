PACKAGE_BASENAME := emake
.DEFAULT_GOAL: help

include emake.mk

emake.mk:
	$(CURL) -O 'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.mk'

test: lint-checkdoc lint-package-lint ## run various linting tools
