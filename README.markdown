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

Better instructions for [Linux]:<http://wiki.github.com/mwotton/Hubris/installation-of-ghc-6121-on-ubuntu-910>
[Mac]: <http://wiki.github.com/mwotton/Hubris/installation-of-ghc-6121-on-mac-os-x>

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
