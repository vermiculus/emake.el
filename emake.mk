# Minimum configuration
#   PACKAGE_BASENAME: the basename that prefixes all Lisp files in this package

ifndef PACKAGE_BASENAME
$(error PACKAGE_BASENAME is not set)
endif

ifndef EMAKE_SHA1
$(error EMAKE_SHA1 is not set)
endif

# User options
#   EMACS_ARGS: [extra arguments for each invocation of emacs]
#   (below; see README)
PACKAGE_FILE            ?= $(PACKAGE_BASENAME).el
PACKAGE_LISP            ?= $(filter-out %-pkg.el %-autoloads.el, $(wildcard $(PACKAGE_BASENAME)*.el))
PACKAGE_TESTS           ?= $(wildcard test/*.el)
PACKAGE_ARCHIVES        ?= gnu
PACKAGE_TEST_ARCHIVES   ?= gnu

EMACS ?= emacs

# Then, make it easy to invoke Emacs with EMake loaded.
EMAKE = PACKAGE_FILE="$(PACKAGE_FILE)" \
	PACKAGE_LISP="$(PACKAGE_LISP)" \
	PACKAGE_TESTS="$(PACKAGE_TESTS)" \
	PACKAGE_ARCHIVES="$(PACKAGE_ARCHIVES)" \
	PACKAGE_TEST_DEPS="$(PACKAGE_TEST_DEPS)" \
	PACKAGE_TEST_ARCHIVES="$(PACKAGE_TEST_ARCHIVES)" \
	$(EMACS) -batch -l emake.el \
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

emake-help: emacs emake.el      ## summarize all targets defined by EMake
	@$(EMAKE) help

help-%: emacs emake.el          ## show help for EMake target '%'
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
	$(CURL) -O 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.el'

emacs-travis.mk:                ## download the emacs-travis.mk Makefile
	$(CURL) -O 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'

.elpa: emake.el                 ## install dependencies as determined by EMake
	$(EMAKE) install

emacs: emake.el                 ## report emacs version (installing $EMACS_VERSION if necessary)
	$(EMACS) -batch -l emake.el -f emake-verify-version 2>&1 || $(MAKE) install-emacs
	$(EMACS) --version

install-emacs: emacs-travis.mk	## build and install a fresh emacs
	export PATH="$(HOME)/bin:$(PATH)"
	make -f emacs-travis.mk install_emacs
