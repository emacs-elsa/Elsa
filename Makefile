EMAKE_SHA1            ?= d8ab7790f5823547fd4e7aec2218771a3dda0638
PACKAGE_BASENAME      := elsa
PACKAGE_FILE          := elsa-pkg.el

# override defaults
PACKAGE_ARCHIVES      := gnu melpa
PACKAGE_TESTS         := $(wildcard tests/*.el)
PACKAGE_TEST_ARCHIVES := gnu melpa

include emake.mk

.DEFAULT_GOAL: help
.PHONY: clean clean-elc test setup

setup: emacs emake.mk

emake.mk:                       ## download EMake
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.mk'

clean-elc:
	rm -f $(PACKAGE_LISP:.el=.elc)

clean: clean-elc
	rm -rf $(EMAKE_WORKDIR)

compile-noerr: clean-elc        ## override: compile, but do not fail on warnings
	$(EMAKE) compile

test: test-buttercup

lint-elsa: PACKAGE_TEST_DEPS += dash trinary buttercup f

emacs: SHELL := /bin/bash
emacs:
	bash -e <(curl -fsSkL 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/build-emacs')
