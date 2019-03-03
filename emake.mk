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

ifndef PACKAGE_BASENAME
$(error PACKAGE_BASENAME is not set)
endif

ifndef EMAKE_SHA1
$(error EMAKE_SHA1 is not set)
endif

EMAKE_WORKDIR ?= .emake

# User options
#   EMACS_ARGS: [extra arguments for each invocation of emacs]
#   (below; see README)

# if we weren't given a PACKAGE_FILE, try to detect one
ifndef PACKAGE_FILE
# If BASENAME-pkg.el exists, use that
ifneq (,$(wildcard $(PACKAGE_BASENAME)-pkg.el))
PACKAGE_FILE := $(PACKAGE_BASENAME)-pkg.el
$(info Using PACKAGE_FILE=$(PACKAGE_FILE))
# otherwise,
else
$(info ./$(PACKAGE_BASENAME)-pkg.el does not exist)
# look for BASENAME.el
ifneq (,$(wildcard $(PACKAGE_BASENAME).el))
# if that exists, use it
PACKAGE_FILE := $(PACKAGE_BASENAME).el
$(info Using PACKAGE_FILE=$(PACKAGE_FILE))
else
# otherwise we're out of ideas
$(info ./$(PACKAGE_BASENAME).el does not exist)
# BASENAME.el exists?
endif
# BASENAME-pkg.el exists?
endif
# PACKAGE_FILE defined?
endif

ifeq (,$(wildcard $(PACKAGE_FILE))) # if PACKAGE_FILE doesn't exist, error out
ifeq (,$(PACKAGE_FILE))		    # if PACKAGE_FILE is null, we tried to detect it above
$(info Could not detect PACKAGE_FILE; has it been created?)
else				# otherwise, we've been given bad configuration
$(info './$(PACKAGE_FILE)' does not exist)
endif
$(error cannot continue without PACKAGE_FILE)
endif

PACKAGE_LISP            ?= $(filter-out %-pkg.el %-autoloads.el, $(wildcard $(PACKAGE_BASENAME)*.el))
PACKAGE_TESTS           ?= $(wildcard test/*.el)
PACKAGE_ARCHIVES        ?= gnu
PACKAGE_TEST_ARCHIVES   ?= gnu

EMACS ?= emacs
EMAKE_LOGLEVEL ?= INFO

EMAKE_ENV = PACKAGE_FILE="$(PACKAGE_FILE)" \
	PACKAGE_LISP="$(PACKAGE_LISP)" \
	PACKAGE_TESTS="$(PACKAGE_TESTS)" \
	PACKAGE_ARCHIVES="$(PACKAGE_ARCHIVES)" \
	PACKAGE_TEST_DEPS="$(PACKAGE_TEST_DEPS)" \
	PACKAGE_TEST_ARCHIVES="$(PACKAGE_TEST_ARCHIVES)" \
	EMAKE_LOGLEVEL="$(EMAKE_LOGLEVEL)" \
	EMAKE_WORKDIR="$(EMAKE_WORKDIR)"

# Then, make it easy to invoke Emacs with EMake loaded.
EMAKE = $(EMAKE_ENV) $(EMACS) --quick --batch --load '$(EMAKE_WORKDIR)/emake.el' \
	--eval "(setq enable-dir-local-variables nil)" \
	$(EMACS_ARGS) \
	--eval "(emake (pop argv))"

CURL ?= curl -fsSkL --retry 9 --retry-delay 9

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

autoloads: emake ## generate autoloads file
	$(EMAKE) autoloads

## Commands useful for Travis

setup: emake ## install EMake

install: $(EMAKE_WORKDIR)/elpa ## install dependencies as determined by EMake

compile: $(EMAKE_WORKDIR)/elpa emake ## compile the project
	rm -f $(PACKAGE_LISP:.el=.elc)
	$(EMAKE) compile ~error-on-warn

## Running specific tests

lint-checkdoc: $(EMAKE_WORKDIR)/elpa emake ## checkdoc
	$(EMAKE) test checkdoc

lint-elsa: PACKAGE_TEST_DEPS += elsa
lint-elsa: PACKAGE_TEST_ARCHIVES += melpa
lint-elsa: emake ## The Emacs Lisp Static Analyzer
	$(EMAKE) test elsa

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
	$(CURL) -o '$(EMAKE_WORKDIR)/emake.el' 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.el'

$(EMAKE_WORKDIR)/elpa: $(EMAKE_WORKDIR)/emake.el
	$(EMAKE) install

emake: $(EMAKE_WORKDIR)/emake.el

emake-debug:			## debug with environment variables
	$(EMAKE_ENV) $(EMACS) --load '$(EMAKE_WORKDIR)/emake.el' $(EMACS_ARGS) --eval "(progn (find-file \"$(EMAKE_WORKDIR)/emake.el\") (cd \"..\"))"
