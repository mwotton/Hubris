#!/bin/bash -x
source $HOME/.rvm/scripts/rvm
rvm 1.9.1
gem install bundler # i am aware how awful this is.
bundle install
rake compile
rake
