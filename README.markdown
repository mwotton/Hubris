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

UPDATE: so it now works for fixed length integers, strings and floats.

anyway, as you can see in the spec, you can use it a little like this:

    class Target
      include Hubris
      def negate(i)
        return -i
      end
    end

    t = Target.new
    t.inline "mydouble (T_FIXNUM i) = T_FIXNUM (i + i)
    my_double _ = T_NIL"
    t.inline "mytriple (T_FIXNUM i) = T_FIXNUM (i * 3)
    mytriple _ = T_NIL"
    puts t.negate(3)
    puts t.mydouble(i)
    puts t.mytriple(i)

There are a few restrictions. All functions take one argument and
return one value: this shouldn't be a major problem because you can
pass arrays of arguments in if you need more. Hubris can currently
handle numbers, strings, basic types (like nil, true and false), and
arrays. Hashes are next, but there will probably be some Ruby
structures (modules, regular expressions, etc) that won't ever be 
handled natively unless someone can convince me it's a sensible thing
to do.

## Requirements

* haskell platform (or ghc-6.8.2 or better and c2hs)
* ruby 1.8.6 or higher
* Linux (Mac OS X is temporarily broken because of dynlibs on
  GHC. High priority to get this working.)
* zsh or bash
* git

## Install

 
    sudo gem install rake open4 rspec
    # get c2hs
    cabal install c2hs
    http://www.haskell.org/ghc/dist/current/dist/ghc-6.11.20090913-src.tar.bz2
    cd ghc-6.11.20090913
    # adjust the argument to -j to your number of cores, and the prefix if you need to install somewhere else
    sh boot && ./configure --enable-shared --prefix=/usr/local && make -j 4 && sudo make install
    cd ..
    git clone git://github.com/mwotton/Hubris.git
    cd Hubris
    rake
    # here's where you'll see a whole lot of successes, if you're very lucky
    # There's a good chance you won't. Tell me what went wrong and i'll fix the docs.
    spec spec/*_spec.rb

If the GHC tarball doesn't work, you might have to get the latest
version from darcs. Try the recipe above first, though.

    wget http://darcs.haskell.org/ghc-HEAD-2009-09-09-ghc-corelibs.tar.bz2
    # WARNING this tarball does not currently work. You'll need to
    # pull the latest code from the darcs GHC repo. I'll update this
    # when a new source tarball that works is up.
    tar -jxvf ghc-HEAD-2009-09-09-ghc-corelibs.tar.bz2
    cd ghc-HEAD-2009-09-09
    # have to get the latest ghc stuff, sadly.
    darcs pull -a
    ./darcs-all get


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

## Not exactly deprecated

JHC was the first compiler Hubris ran under, but it's lagging a bit
now because I've been focused on getting GHC going. This is probably
for interested hackers only now.

- you may have to run "sudo cabal install binary zlib utf8-string readline fgl" - am not entirely sure what's in the Haskell
  Platform. Feedback on this step appreciated.
## JHC Install

- JHC is thankfully now a little easier to build. Follow the instructions at http://repetae.net/computer/jhc/building.shtml - the
  most recent tested working version is http://repetae.net/dist/jhc-0.7.1.tar.gz.

        ./configure && make && sudo make install

should do the trick.

- this will copy the jhc binary to /usr/local/bin. If it's not in your
  PATH already, add "export PATH=$PATH:/usr/local/bin" to your .bashrc.

[haskell_platform]: http://hackage.haskell.org/platform/
[jhc]: http://repetae.net/computer/jhc/
