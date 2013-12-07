all: test

test: ecukes

ecukes:
	cask exec ecukes

.PHONY: ecukes test all
