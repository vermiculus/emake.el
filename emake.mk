# Minimum configuration
#   PACKAGE_BASENAME: the basename that prefixes all Lisp files in this package
#   EMAKE_SHA1:       the version of emake you want to use
#   EMACS_VERSION:    the version of emacs you're using for testing

ifndef PACKAGE_BASENAME
$(error PACKAGE_BASENAME is not set)
endif

ifndef EMAKE_SHA1
$(error EMAKE_SHA1 is not set)
endif

ifndef EMACS_VERSION
$(error EMACS_VERSION is not set)
endif

EMAKE_WORKDIR ?= .emake

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
	EMAKE_WORKDIR="$(EMAKE_WORKDIR)" \
	$(EMACS) -batch -l '$(EMAKE_WORKDIR)/emake.el' \
	--eval "(setq enable-dir-local-variables nil)" \
	$(EMACS_ARGS) \
	--eval "(emake (pop argv))"

CURL ?= curl --fail --silent --show-error --insecure --location --retry 9 --retry-delay 9

# Set up our phony targets so Make doesn't think there are files by
# these names.
.PHONY: setup install compile help help-% emacs install-emacs emake

### EMake-released targets

help: ## show help
#	Reference: https://stackoverflow.com/a/33206814/1443496
	@grep -hE '(^[A-Za-z_/%\.\-]+:.*?##.*$$)|(^##.*$$)' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}{printf ($$2=="" ? "\033[32m%s\033[0m\n" : "\033[32m%-30s\033[0m %s\n"), $$1, $$2}' \
		| sed -e 's/\[32m### */[31;1;4m/' \
		| sed -e 's/\[32m## */[33;4m/'

emake-help: emake ## summarize all targets defined by EMake
	@$(EMAKE) help

help-%: emake ## show help for EMake target '%'
	$(EMAKE) help $*


## Commands useful for Travis

setup: emake ## install emacs/emake

install: $(EMAKE_WORKDIR)/elpa ## install dependencies as determined by EMake

compile: $(EMAKE_WORKDIR)/elpa emake ## compile the project
	rm -f $(PACKAGE_LISP:.el=.elc)
	$(EMAKE) compile ~error-on-warn

## Running specific tests

lint-checkdoc: $(EMAKE_WORKDIR)/elpa emake ## checkdoc
	$(EMAKE) test checkdoc

lint-package-lint: PACKAGE_TEST_DEPS += package-lint
lint-package-lint: PACKAGE_TEST_ARCHIVES += melpa
lint-package-lint: $(EMAKE_WORKDIR)/elpa emake ## package-lint
	$(EMAKE) test package-lint

test-buttercup: PACKAGE_TEST_DEPS += buttercup
test-buttercup: PACKAGE_TEST_ARCHIVES += melpa
test-buttercup: $(EMAKE_WORKDIR)/elpa emake ## buttercup
	$(EMAKE) test buttercup

test-ert: $(EMAKE_WORKDIR)/elpa ## ERT
	$(EMAKE) test ert

# Support targets

$(EMAKE_WORKDIR):
	mkdir -p $(EMAKE_WORKDIR)

$(EMAKE_WORKDIR)/emake.el: $(EMAKE_WORKDIR)
	$(CURL) 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.el' \
	  --output '$(EMAKE_WORKDIR)/emake.el'

$(EMAKE_WORKDIR)/emacs-travis.mk: $(EMAKE_WORKDIR)
	$(CURL) 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk' \
	  --output '$(EMAKE_WORKDIR)/emacs-travis.mk'

$(EMAKE_WORKDIR)/elpa: $(EMAKE_WORKDIR)/emake.el
	$(EMAKE) install

emake: emacs $(EMAKE_WORKDIR)/emake.el
emacs: $(EMAKE_WORKDIR)/emake.el
	$(EMACS) -batch -l '$(EMAKE_WORKDIR)/emake.el' -f emake-verify-version || $(MAKE) install-emacs
	$(EMACS) --version

install-emacs: $(EMAKE_WORKDIR)/emacs-travis.mk
	export PATH="$(HOME)/bin:$(PATH)"
	make -f '$(EMAKE_WORKDIR)/emacs-travis.mk' install_emacs
