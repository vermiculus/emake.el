# Copyright (C) 2018-2019  Sean Allred

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


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

EMAKE_ENV = PACKAGE_FILE="$(PACKAGE_FILE)" \
	PACKAGE_LISP="$(PACKAGE_LISP)" \
	PACKAGE_TESTS="$(PACKAGE_TESTS)" \
	PACKAGE_ARCHIVES="$(PACKAGE_ARCHIVES)" \
	PACKAGE_TEST_DEPS="$(PACKAGE_TEST_DEPS)" \
	PACKAGE_TEST_ARCHIVES="$(PACKAGE_TEST_ARCHIVES)" \
	EMAKE_WORKDIR="$(EMAKE_WORKDIR)"

# Then, make it easy to invoke Emacs with EMake loaded.
EMAKE = $(EMAKE_ENV) $(EMACS) --quick --batch --load '$(EMAKE_WORKDIR)/emake.el' \
	--eval "(setq enable-dir-local-variables nil)" \
	$(EMACS_ARGS) \
	--eval "(emake (pop argv))"

CURL ?= curl --fail --silent --show-error --insecure --location --retry 9 --retry-delay 9

# Set up our phony targets so Make doesn't think there are files by
# these names.
.PHONY: setup install compile help help-% emacs install-emacs install-emacs-travis install-evm emake

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

setup: emacs emake ## install emacs/emake

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

emake: $(EMAKE_WORKDIR)/emake.el

ifeq ($(CI),true)
emacs: install-emacs
	$(EMACS) --version
else
emacs: $(EMAKE_WORKDIR)/emake.el
	$(EMACS) -batch -l '$(EMAKE_WORKDIR)/emake.el' -f emake-verify-version || $(error Wrong version!)
endif

emake-debug:			## debug with environment variables
	$(EMAKE_ENV) $(EMACS) --load '$(EMAKE_WORKDIR)/emake.el' $(EMACS_ARGS) --eval "(progn (find-file \"$(EMAKE_WORKDIR)/emake.el\") (cd \"..\"))"

# Installing Emacs on CI - use EVM where we can; emacs-travis elsewhere
ifneq ($(CI),true)
# We probably don't want to do this outside CI
install-emacs:
	$(error Refusing to install emacs outside CI)
else

# Determine if we can use EVM.  In general, it's much faster to
# install as it uses pre-compiled binaries.
EMAKE_USE_EVM ?= true
# The only times we don't want to do this:
#
#  1) when we're not on Linux (the pre-compiled binaries obviously
#  won't work; they're built for Travis's Linux)
ifeq ($(TRAVIS_OS_NAME),osx)
EMAKE_USE_EVM := false
endif
#
#  2) when we're trying to test on the snapshot build.  EVM simply
#  doesn't pull straight from git -- it uses pre-compiled binaries.
#  (That's kinda the point.)  It's incredibly slow, but at least it
#  works correctly!
ifeq ($(EMACS_VERSION),snapshot)
EMAKE_USE_EVM := false
endif

ifeq ($(EMAKE_USE_EVM),true)
export PATH := $(HOME)/.evm/bin:$(PATH)
install-emacs: install-evm
ifeq ($(TRAVIS),true)
# Travis has special Docker needs
	evm config path /tmp
	evm install emacs-$(EMACS_VERSION)-travis --use --skip
else
	evm install emacs-$(EMACS_VERSION) --use --skip
endif
else
export PATH := $(HOME)/bin:$(PATH)
install-emacs: install-emacs-travis
	make -f '$(EMAKE_WORKDIR)/emacs-travis.mk' install_emacs
endif
endif

install-emacs-travis: $(EMAKE_WORKDIR)/emacs-travis.mk

install-evm:
ifeq ($(wildcard "$(HOME)/.evm/."),)
	git clone "https://github.com/rejeep/evm.git" "$(HOME)/.evm"
endif
