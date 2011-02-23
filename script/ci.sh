
#!/bin/bash -x
ghc_version=$1
set -e
export PATH=$PATH:$HOME/.cabal/bin
# another huge hack. let's fix this properly soon TODO
# export LD_LIBRARY_PATH=$HOME/.rvm/rubies/ruby-1.9.1-p378/lib/    
export HUBRIS_DIR=~/tmp
mkdir $HUBRIS_DIR || true
rm -rf $HUBRIS_DIR/* || true
rm -rf /tmp/hubris* ||true
cabal install zlib --user  --enable-shared # needed for tests

# delete old haskell stuff
ghc-pkg-$ghc_version unregister hubris || true
rm `which Hubrify` || true
rvm 1.9.1
gem build hubris.gemspec
gem install hubris-0.0.4.gem # can we do these two in one step?

#gem install bundler # i am aware how awful this is.
#bundle install
#bundle exec rake compile
#rm -rf $HUBRIS_DIR/*
#bundle exec rake
