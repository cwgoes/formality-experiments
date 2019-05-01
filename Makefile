all: setup build

setup:
	stack build --only-dependencies

build:
	stack build --copy-bins --fast

lint:
	stack exec -- hlint app src

test:
	stack test --fast

repl:
	stack ghci formality-experiments:lib

clean:
	stack clean

.PHONY: all setup build lint test repl clean
