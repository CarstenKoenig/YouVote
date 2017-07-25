# YouVote

right now this is only a simple *proof of concept*
letting you add two numbers (yeah I know - high science)

## getting started
if you have a **bash** shell available you can just use the

    ./make.sh
	
script to get started - if not just follow these steps:	

### make

alternatively there is an `Makefile` included so

    make server-start
	
should compile everything and then fire up the server	

### building the server app
the first step is to get the Haskell appliction compiled:

    stack build
	
of course you want to have **stack** installed for this.

Also this could take quite some time if you don't have the dependencies cached alread - so maybe get a coffee.
	
### generating the servant-api for elm
next we need have **servant** generate the `api.elm` for us:

    stack exec YouVote-codeGen
	
this should give you `src/client/api.elm`	
	
###	compiling the elm application
now we can let **elm** generate the `static/js/client.js` for us:

	cd src/client
    elm make Main.elm --output ../../static/js/client.js
	cd ../..
	
**all done**	

### starting
just do

	stack exec YouVote
	
this should start a server at [localhost:8080](http://localhost:8080)	
