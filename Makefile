EMAKE_SHA1 := fake-hash		# hack to make tests work
EMACS_VERSION ?= 26.1
PACKAGE_BASENAME := emake

.DEFAULT_GOAL: help
.PHONY: $(EMAKE_WORKDIR)/emake.el # remake every time

include emake.mk

# redeclare how to create emake.el
$(EMAKE_WORKDIR)/emake.el:
	cp emake.el $(EMAKE_WORKDIR)/emake.el

test: lint-checkdoc lint-package-lint ## run various linting tools

clean:
	rm -rf $(EMAKE_WORKDIR)
