setup:
	stack setup

build:
	stack build --fast --test --no-run-tests

test:
	stack test

.DEFAULT_GOAL := build
