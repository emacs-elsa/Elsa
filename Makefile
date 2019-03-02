# EMACS_VERSION should be set in your ~/.profile on your development machine
EMAKE_SHA1            ?= ef8e31c979ca6d084f20ccc975c5fc6d22dd31b9
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

compile: clean-elc		## override: compile, but do not fail on warnings
	$(EMAKE) compile

test: test-buttercup

emacs: SHELL := /bin/bash
emacs:
	bash -e <(curl -fsSkL 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/install-emacs')
