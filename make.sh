#!/bin/bash
set -e

clear

printf "\n\n------\n"
printf "building application...\n"
stack build

printf "\n\n------\n"
printf "elm code-gen...\n"
stack exec YouVote-codeGen

printf "\n\n------\n"
printf "compiling elm app...\n"
cd src/client
elm make Main.elm --output ../../static/js/client.js
cd ../..

printf "\n\n=========\n"
printf "starting server...\n"
stack exec YouVote
