#!/bin/bash -x
set -e
source $HOME/.rvm/scripts/rvm
export PATH=$PATH:$HOME/.cabal/bin
# another huge hack. let's fix this properly soon TODO
export LD_LIBRARY_PATH=$HOME/.rvm/rubies/ruby-1.9.1-p378/lib/    

# reinstall haskell stuff
ghc-pkg unregister hubris || true
# rm `which Hubrify`
cd Haskell
cabal install --extra-include-dirs=/home/mwotton/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/x86_64-linux --extra-include-dirs=/home/mwotton/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/ --extra-lib-dirs=/home/mwotton/.rvm/rubies/ruby-1.9.1-p378/lib/ --user  --enable-shared  
cd ..


rvm 1.9.1
gem install bundler # i am aware how awful this is.
bundle install
rake compile
rm /var/hubris/*/* /tmp/hubris*
rake
