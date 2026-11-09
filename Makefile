# Run these inside the development environment: guix shell -m manifest.scm

SHELL := bash
EGGS  := sdl2 srfi-151 srfi-18 miscmacros bitstring test args
ROM   ?= 1-chip8-logo.ch8
ENV   := . scripts/env.sh &&

.PHONY: help shell eggs test run repl clean-eggs

help:
	@echo "make shell       enter the Guix development environment"
	@echo "make eggs        install the CHICKEN eggs into .chicken/eggs"
	@echo "make test        run the test suite"
	@echo "make run ROM=f   start the emulator with ROM f (default $(ROM))"
	@echo "make repl        start csi with the project eggs available"
	@echo "make clean-eggs  remove .chicken/"

shell:
	guix shell -m manifest.scm

eggs:
	$(ENV) chicken-install $(EGGS)

test:
	$(ENV) csi -s tests/run.scm

run:
	$(ENV) csi -s src/main.scm -i $(ROM)

repl:
	$(ENV) csi

clean-eggs:
	rm -rf .chicken
