
all: setup build

setup: server-setup client-setup

build: server-build server-codegen client-build

client-setup:
	(cd src/client ; elm package install -y ; cd ../..)

client-build:
	(cd src/client ; elm make Main.elm --output ../../static/js/client.js ; cd ../..)

server-setup:
	stack setup
	stack test --only-dependencies

server-build:
	stack build

server-codegen: server-build
	stack exec YouVote-codeGen

server-start: build
	stack exec YouVote

test: build
	stack test

clean:
	stack clean
	rm -f ./static/js/client.js
	rm -f ./src/client/Api.elm
