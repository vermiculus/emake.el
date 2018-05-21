EENVS := PACKAGE_FILE="sample.el" PACKAGE_ARCHIVES="gnu melpa" PACKAGE_LISP="sample.el"
EMAKE := $(EENVS) emacs -batch -l emake.el --eval "(emake (pop argv))"

.PHONY: clean setup install compile test

emake.el:
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.el'

emacs-travis.mk:
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'

clean:
	rm -f *.elc
	rm -rf .elpa/

setup: emacs emake.el
	export PATH="$(HOME)/bin:$(PATH)"
	make -f emacs-travis.mk install_emacs

install:
	$(EMAKE) install

compile: clean
	$(EMAKE) compile

test:
	$(EMAKE) test
