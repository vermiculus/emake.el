EENVS  = PACKAGE_FILE="emake.el"
EENVS += PACKAGE_LISP="emake.el"
EENVS += PACKAGE_TEST_DEPS="package-lint"
EENVS += PACKAGE_TEST_ARCHIVES="melpa"

EMAKE := $(EENVS) emacs -batch -l emake.el --eval "(emake (pop argv))"

.PHONY: clean setup install compile test

emacs-travis.mk:		## emacs install script
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'

clean:				## clean up generated files
	rm -f *.elc
	rm -rf .elpa/
	rm -rf .elpa.test/
	rm -f emacs-travis.mk

install:			## install the package
	$(EMAKE) install

compile:			## recompile the package
	rm -f *.elc
	$(EMAKE) compile ~error-on-warn

test:				## run various linting tools
	$(EMAKE) test checkdoc
	$(EMAKE) test package-lint

ifeq ($(CI),true)
emacs: emacs-travis.mk		## install emacs on Travis
	export PATH="$(HOME)/bin:$(PATH)"
	make -f emacs-travis.mk install_emacs
else
emacs:				## report local installation information
	which emacs && emacs --version
endif
