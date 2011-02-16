
#!/bin/bash -x
ghc_version=$1
set -e
#source $HOME/.rvm/scripts/rvm
export PATH=$PATH:$HOME/.cabal/bin
# another huge hack. let's fix this properly soon TODO
export LD_LIBRARY_PATH=$HOME/.rvm/rubies/ruby-1.9.1-p378/lib/    
export HUBRIS_DIR=~/tmp
mkdir $HUBRIS_DIR || true
rm -rf $HUBRIS_DIR/* || true
rm -rf /tmp/hubris* ||true
cabal install zlib # needed for tests
# source $HOME/.rvm/scripts/rvm || true
# rvm reload

# rm `which Hubrify`
cd Haskell
#cabal install --with-ghc=ghc-$ghc_version ||true
# reinstall haskell stuff
ghc-pkg-$ghc_version unregister hubris || true

#rm -rf hint
#darcs clone http://code.haskell.org/hint/devel hint
#cd hint
#cabal install --with-ghc=ghc-$ghc_version
#cd ..

# ghc-$ghc_version --make Setup
# this is pretty ugly - this line creates the Includes.hs file, 
# as cabal install ignores the given Setup.hs. FIXME
# ./Setup configure --extra-include-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/x86_64-linux --extra-include-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/ --extra-lib-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/lib/ --user  --enable-shared  
cabal install --extra-include-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/x86_64-linux --extra-include-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/ --extra-lib-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/lib/ --user  --enable-shared  --with-ghc=ghc-$ghc_version
cd ..


rvm 1.9.1
gem install bundler # i am aware how awful this is.
bundle install
bundle exec rake compile
rm -rf $HUBRIS_DIR/*
bundle exec rake
