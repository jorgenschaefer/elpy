.PHONY: all test unit-test ecukes test-all tox clean cask

all: test

test: unit-test ecukes

unit-test:
	cask exec ert-runner --quiet

ecukes:
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
