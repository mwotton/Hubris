
#!/bin/bash -x
ghc_version=$1
set -e
export PATH=$PATH:$HOME/.cabal/bin
# another huge hack. let's fix this properly soon TODO
export LD_LIBRARY_PATH=$HOME/.rvm/rubies/ruby-1.9.1-p378/lib/    
export HUBRIS_DIR=~/tmp
mkdir $HUBRIS_DIR || true
rm -rf $HUBRIS_DIR/* || true
rm -rf /tmp/hubris* ||true
cabal install zlib --user  --enable-shared # needed for tests

rm `which Hubrify` || true
cd Haskell

# reinstall haskell stuff
ghc-pkg-$ghc_version unregister hubris || true

cabal install --extra-include-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/x86_64-linux --extra-include-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/include/ruby-1.9.1/ --extra-lib-dirs=$HOME/.rvm/rubies/ruby-1.9.1-p378/lib/ --user  --enable-shared  --with-ghc=ghc-$ghc_version
cd ..

rvm 1.9.1
gem install bundler # i am aware how awful this is.
bundle install
bundle exec rake compile
rm -rf $HUBRIS_DIR/*
bundle exec rake
