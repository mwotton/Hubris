# Hubris

## Description

Hubris is a bridge between Ruby and Haskell, between love and bondage,
between slothful indolence and raw, blazing speed. Hubris will wash
your car, lie to your boss, and salvage your love life. If you are 
very, very lucky, it might also let you get some functional goodness 
into your ruby programs through the back door.

I probably don't have to say this, but patches are very much
welcome. If you have trouble installing it, tell me, and help me
improve the docs.

## Synopsis

The best docs, as ever, are in the tests, but as a quick precis, you
can use it a little like this:

    require 'hubris' # best line ever

    class Target
      hubris :inline =>"triple::Int->Int; triple n = 3*n"
    end

    t = Target.new
    puts t.triple(10)
       => 30

There are a few restrictions. All functions take one argument and
return one value: this shouldn't be a major problem because you can
pass arrays of arguments in if you need more. Hubris can currently
handle numbers, strings, basic types (like nil, true and false),
arrays and hashes. There will probably be some Ruby structures
(modules, regular expressions, etc) that won't ever be handled
natively unless someone can convince me it's a sensible thing to do.

Hubris will refuse to compile Haskell code that produces any
warnings. You can suppress this admittedly fairly strict behaviour by
passing the ":no_strict => true" flag, but in your heart of hearts
you'll know you've done the wrong thing.

There are also two other modes:

      hubris :source => "MyCoolModule.hs"

which loads a source file on disk (in the same directory as your ruby),
and

      hubris :module => "Data.ByteString", :packages => ["bytestring"]

which will load the Data.ByteString module which is installed on the
system. In this case, we also need to let the Haskell side know that
we'll be using the "bytestring" package, so we pass that too: You may
need to load extra packages with :inline and :source as well, and
that's supported.


## Requirements

* ghc 6.10 (to bootstrap 6.12) and cabal-install. This comes with the
  Haskell Platform
* ruby 1.8.6 or higher (most heavily tested on 1.9.1)
* Linux or Mac. See
  <http://www.shimweasel.com/2009/09/14/unprincipled-skulduggery-with-ghc-6-12-dylibs-on-mac-os-x>
  and the following entry for more info on the Mac build.
* zsh or bash
* git

## Install

First, we install GHC 6.12 RC2 (living on the cutting edge is fun,
right?)

    wget http://www.haskell.org/ghc/dist/6.12.1-rc2/ghc-6.12.0.20091121-src.tar.bz2
    tar -jxvf ghc-6.12.0.20091121-src.tar.bz2
    cd ghc-6.12.0.20091121-src
    # adjust the argument to -j to your number of cores, and the prefix if you need to install somewhere else
    sh boot && ./configure --enable-shared --prefix=/usr/local && make    -j 4 && sudo make install
    # check ghc --version at the prompt tells you you're running 6.12

Then get the Haskell support libraries installed

    cabal install c2hs
    # probably a better way of doing this, but this is how i build it.
    cabal unpack hubris
    cd hubris-0.0.2
    # edit the --extra-include-dirs and --extra-lib-dirs to reflect
    # your installation. You'll need the ruby headers installed -
    # they're installed already in ports, and it's ruby1.9.dev on Ubuntu.
    runhaskell Setup configure  --enable-shared --user --ghc-options=-dynamic --extra-include-dirs=/usr/local/include/ruby-1.9.1/ --extra-lib-dirs=/usr/local/lib/
    runhaskell Setup build
    runhaskell Setup install
    # check that Hubrify is now in your path.

Then the Ruby side

    sudo gem install rake open4 rspec hubris

    git clone git://github.com/mwotton/Hubris.git
    cd Hubris/lib
    ruby extconf.rb && make
    cd ..
    spec .

I'll gemify this soon too.


## Contributors

* Mark Wotton
* James Britt
* Josh Price
* Tatsuhiro Ujihisa

## License

(The MIT License)

Copyright (c) 2009 Mark Wotton

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


[haskell_platform]: http://hackage.haskell.org/platform/
