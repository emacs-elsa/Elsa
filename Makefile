# EMACS_VERSION should be set in your ~/.profile on your development machine
EMACS_VERSION         ?= 26.1
EMAKE_SHA1            ?= 2dec3626e073c3b4aa88fbedd7ad5d27e530f470
PACKAGE_BASENAME      := elsa

# override defaults
PACKAGE_ARCHIVES      := gnu melpa
PACKAGE_TESTS         := $(wildcard tests/*.el)
PACKAGE_TEST_DEPS     := dash
PACKAGE_TEST_ARCHIVES := gnu melpa

.DEFAULT_GOAL: help
.PHONY: clean clean-elc test

emake.mk:                       ## download EMake
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.mk'

clean-elc:
	rm -f $(PACKAGE_LISP:.el=.elc)

clean: clean-elc
	rm -rf $(EMAKE_WORKDIR)

include emake.mk

compile: clean-elc		## override: compile, but do not fail on warnings
	$(EMAKE) compile

test: test-buttercup
