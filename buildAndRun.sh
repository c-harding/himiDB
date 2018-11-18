#!/bin/bash

echo "Build project" 
stack setup && stack build
echo "Running up.."
stack exec himiDB