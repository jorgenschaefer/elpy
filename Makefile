.PHONY: cask cask-update cask-test ert ecukes tox test-warnings test-all clean

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

cask-test:
	cask exec ert-runner --quiet
	EMACS=emacs-24.1 cask exec ert-runner --quiet
	EMACS=emacs-24.2 cask exec ert-runner --quiet
	EMACS=emacs-24.3 cask exec ert-runner --quiet

ert:
	cask exec ert-runner --quiet

ecukes:
	cask exec ecukes --script --quiet

tox:
	tox

test-warnings:
	python -Qwarnall -tt -W error -m unittest discover elpy

test-all: test-warnings tox

clean:
	find ./* -name '*.pyc' -delete
	find ./* -name '*.elc' -delete
	find ./* -name __pycache__ -delete
