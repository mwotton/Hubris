#!/bin/bash -x
source $HOME/.rvm/scripts/rvm
rvm 1.9.1
bundle install
rake compile
rake
