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

python-test:
	python -Qwarnall -tt -W error -m unittest discover elpy

python-test-all:
	tox

test-all: test-warnings tox

clean:
	find ./* -name '*.pyc' -delete
	find ./* -name '*.elc' -delete
	find ./* -name __pycache__ -delete
