#!/bin/bash

echo "Test checking"
stack test 
echo "Build project" 
stack setup && stack build
echo "Running up.."
stack exec himiDB
