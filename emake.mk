# Minimum configuration
#   PACKAGE_BASENAME: the basename that prefixes all Lisp files in this package

ifndef PACKAGE_BASENAME
$(error PACKAGE_BASENAME is not set)
endif

# User options
#   EMACS_ARGS: [extra arguments for each invocation of emacs]
#   (below; see README)
PACKAGE_FILE            ?= $(PACKAGE_BASENAME).el
PACKAGE_LISP            ?= $(filter-out %-pkg.el %-autoloads.el, $(wildcard $(PACKAGE_BASENAME)*.el))
PACKAGE_TESTS           ?= $(wildcard test/*.el)
PACKAGE_ARCHIVES        ?= gnu
PACKAGE_TEST_ARCHIVES   ?= gnu

# Then, make it easy to invoke Emacs with EMake loaded.
EMAKE = PACKAGE_FILE="$(PACKAGE_FILE)" \
	PACKAGE_LISP="$(PACKAGE_LISP)" \
	PACKAGE_TESTS="$(PACKAGE_TESTS)" \
	PACKAGE_ARCHIVES="$(PACKAGE_ARCHIVES)" \
	PACKAGE_TEST_DEPS="$(PACKAGE_TEST_DEPS)" \
	PACKAGE_TEST_ARCHIVES="$(PACKAGE_TEST_ARCHIVES)" \
	emacs -batch -l emake.el \
	--eval "(setq enable-dir-local-variables nil)" \
	$(EMACS_ARGS) \
	--eval "(emake (pop argv))"

CURL ?= curl --fail --silent --show-error --insecure --location --retry 9 --retry-delay 9

# Set up our phony targets so Make doesn't think there are files by
# these names.
.PHONY: clean setup install compile test help emacs install-emacs

help:                           ## show help
	@grep -hE '(^[A-Za-z_/%\.\-]+:.*?##.*$$)|(^##.*$$)' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}{printf "\033[32m%-30s\033[0m %s\n", $$1, $$2}' \
		| sed -e 's/\[32m## */[33m/'
help-%:                         ## show help for EMake target '%'
	$(EMAKE) help $*


# Tell Make how to 'clean' this project; use double-colon to allow additions downstream
clean::                         ## clean all generated files
	rm -f *.elc             # delete compiled files
	rm -rf .elpa/           # delete dependencies
	rm -rf .elpa.test/
	rm -f emacs-travis.mk
	rm -f emake.el

## Commands useful for Travis

# Tell Make how to 'setup' this project (e.g., for Travis).  This
# requires both Emacs to be installed and the `emake.el' script to be
# available.
setup: emacs emake.el

compile: .elpa                  ## compile the project
	rm -f $(PACKAGE_LISP:.el=.elc)
	$(EMAKE) compile ~error-on-warn

## Running specific tests

lint-checkdoc: .elpa            ## checkdoc
	$(EMAKE) test checkdoc

lint-package-lint: PACKAGE_TEST_DEPS += package-lint
lint-package-lint: PACKAGE_TEST_ARCHIVES += melpa
lint-package-lint: .elpa        ## package-lint
	$(EMAKE) test package-lint

test-buttercup: PACKAGE_TEST_DEPS += buttercup
lint-package-lint: PACKAGE_TEST_ARCHIVES += melpa
test-buttercup: .elpa           ## buttercup
	$(EMAKE) test buttercup

test-ert: .elpa                 ## ERT
	$(EMAKE) test ert

## Support targets

emake.el:                       ## download the EMake script
	$(CURL) -O 'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.el'

emacs-travis.mk:                ## download the emacs-travis.mk Makefile
	$(CURL) -O 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'

.elpa: emake.el                ## install dependencies as determined by EMake
	$(EMAKE) install

# no emacs or not the right version?  install the right emacs.
ifneq (ok,$(shell emacs -batch -l emake.el -f emake-verify-version 2>&1))
# outputs 'ok' if the major.minor of `emacs-version' is EMACS_VERSION.
# Note if emacs is not in PATH, then we still won't output "ok" (and
# Make won't freak out from the exit code).
emacs: install-emacs
endif
emacs:                          ## report emacs version (installing $EMACS_VERSION if necessary)
	emacs --version

install-emacs: emacs-travis.mk	## build and install a fresh emacs
	export PATH="$(HOME)/bin:$(PATH)"
	make -f emacs-travis.mk install_emacs
