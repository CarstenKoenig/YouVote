#!/bin/bash
clear

echo "building application..."
stack build

echo "elm code-gen..."
stack exec YouVote-codeGen

echo "compiling elm app..."
cd src/client
elm make Main.elm --output ../../static/js/client.js
cd ../..

echo "starting server..."
stack exec YouVote
