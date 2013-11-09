.PHONY: all test unit-tests functional-tests test-all tox clean cask

all: test

test: unit-tests functional-tests

unit-tests:
	cask exec ert-runner --quiet

functional-tests:
	cask exec ecukes --script --quiet

test-all: tox

tox:
	tox

cask:
	cask
	EMACS=emacs-24.1 cask
	EMACS=emacs-24.2 cask
	EMACS=emacs-24.3 cask

clean:
	find * -name '*.pyc' -exec rm {} +
	find * -name '*.elc' -exec rm {} +
	find -name __pycache__ -exec rmdir {} +


VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' elpy.el)
BUILDDIR="dist/elpy-$(VERSION)"

marmalade:
	test -d $(BUILDDIR) && rm -rf $(BUILDDIR) || true
	mkdir -p $(BUILDDIR)

	install -m 644 elpy.el elpy-refactor.el elpy-pkg.el LICENSE $(BUILDDIR)
	install -m 644 README.rst $(BUILDDIR)/README
	cp -r snippets $(BUILDDIR)/

	tar -C "dist/" -c elpy-$(VERSION) > dist/elpy-$(VERSION).tar
	rm -rf $(BUILDDIR)
