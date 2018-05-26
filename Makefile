EENVS  = PACKAGE_FILE="emake.el"
EENVS += PACKAGE_LISP="emake.el"
EENVS += PACKAGE_TEST_DEPS="package-lint"
EENVS += PACKAGE_TEST_ARCHIVES="melpa"

EMAKE := $(EENVS) emacs -batch -l emake.el --eval "(emake (pop argv))"

.PHONY: clean setup install compile test

emacs-travis.mk:
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'

clean:
	rm -f *.elc
	rm -rf .elpa/
	rm -rf .elpa.test/
	rm -f emacs-travis.mk

setup: emacs

install:
	$(EMAKE) install

compile:
	rm -f *.elc
	$(EMAKE) compile ~error-on-warn

test:
	$(EMAKE) test checkdoc
	$(EMAKE) test package-lint

ifeq ($(CI),true)
emacs: emacs-travis.mk
	export PATH="$(HOME)/bin:$(PATH)"
	make -f emacs-travis.mk install_emacs
else
emacs:
	which emacs && emacs --version
endif
