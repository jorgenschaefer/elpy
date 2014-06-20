.PHONY: test update cask cask-update elisp-test elisp-test-all python-test python-test-all test-all clean

test: python-test elisp-test

update: cask cask-update

cask:
	cask
	EMACS=emacs-24.1 cask
	EMACS=emacs-24.2 cask
	EMACS=emacs-24.3 cask

cask-update:
	cask update
	EMACS=emacs-24.1 cask update
	EMACS=emacs-24.2 cask update
	EMACS=emacs-24.3 cask update

elisp-test:
	cask exec ert-runner --quiet

elisp-test-all:
	cask exec ert-runner --quiet
	EMACS=emacs-24.1 cask exec ert-runner --quiet
	EMACS=emacs-24.2 cask exec ert-runner --quiet
	EMACS=emacs-24.3 cask exec ert-runner --quiet

elisp-coverage:
	@echo "Missing tests:" ; cat elpy.el | sed -ne 's/^(\(defun\|define-minor-mode\|defmacro\|defsubst\) \([^ ]*\).*/\2/p' | while read fun ; do test -f "test/$${fun}-test.el" || echo "- $$fun" ; done

python-test:
	python -Qwarnall -tt -W error -m unittest discover elpy

python-test-all:
	tox

python-coverage:
	coverage run --source=elpy --branch -m unittest discover
	coverage html --omit='elpy/tests/*,elpy/compat*,elpy/__main__*' -d ~/Public/elpy-coverage.html
	coverage report -m --fail-under=95 --omit='elpy/tests/*,elpy/compat*,elpy/__main__*'

test-all: test-warnings tox

VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' elpy.el)

tar: clean
	mkdir -p build/elpy-${VERSION} dist/
	cp -r elpy.el elpy-refactor.el elpy-pkg.el LICENSE README.rst \
	      snippets/ elpy/ build/elpy-${VERSION}/
	rm -rf build/elpy-${VERSION}/elpy/tests/
	tar -C build -c elpy-${VERSION} > dist/elpy-${VERSION}.tar

clean:
	find ./* -name '*.pyc' -delete
	find ./* -name '*.elc' -delete
	find ./* -name __pycache__ -delete
	rm -rf .coverage build/ dist/ elpy.egg-info/
