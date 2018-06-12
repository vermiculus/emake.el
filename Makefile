PACKAGE_BASENAME := emake
.DEFAULT_GOAL: help

include emake.mk

test: lint-checkdoc lint-package-lint ## run various linting tools
