#!/usr/bin/env sh
# Delete existing binaries to see that the lib correctly recreates them 
clear; 
rm -rf  ~/.hubris_cache/*; rm -rf *.o; rm -rf *.hi;  rm -rf *.c;  
ruby haskell_math.rb 
