#!/bin/bash

echo "Build project" 
stack setup && stack build
echo "Test checking"
stack test 
echo "Running up.."
stack exec himiDB