#!/bin/bash -x
source $HOME/.rvm/scripts/rvm
export PATH=$PATH:$HOME/.cabal/bin
rvm 1.9.1
gem install bundler # i am aware how awful this is.
bundle install
rake compile
rm /var/hubris/*/* /tmp/hubris*
rake
