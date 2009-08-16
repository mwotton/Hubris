# Hubris

## Description

This is a quick and dirty way to call Haskell functions from ruby.

Hubris will wash your car, lie to your boss, and salvage your love life.
If you are very, very lucky, it might also let you get some uber-fast
functional goodness into your ruby programs through the back door.

## Synopsis

Eventually, we'll integrate with RubyInline or something similar, 
so we can write inline Haskell. Until that happy day:

* write a haskell file (say sample/foo.hs) with some ccall exports declared
* call "jhc_builder.sh foo.hs". This will build "libfoo.so".
* write a ruby file similar to sample/hsload.rb in order to call the functions from ruby

If all else fails, mail mwotton@gmail.com with tales of woe.

## Requirements


* jhc (John's Haskell Compiler)
* gcc (oh, come on. don't tell me you don't have it)
* ruby 1.8.6 or higher
* Mac OSX or Linux
* bash

## Install

1. Install the Hubris gem from RubyForge

<code>
    sudo gem install hubris
</code>

2. Or live on the bleeding edge and install the latest from Github

<code>
    gem source --add http://gems.github.com
    sudo gem install mwotton-hubris
</code>

3. Get the [Haskell Platform][haskell_platform] and install this for your platform (now we have GHC for building JHC)

4. Install Darcs (so we can access the JHC repository)

<code>
    sudo cabal update
    sudo cabal install --global darcs
</code>

  Don't worry too much about any warnings that you may see while this builds.

5. Install [JHC][jhc] (the instructions there are slightly out of date so use the following instead)

<code>
    darcs get http://repetae.net/repos/jhc
    cd jhc/src
    darcs get --partial http://repetae.net/repos/Doc
    cd ../lib
    darcs get --partial http://darcs.haskell.org/packages/haskell98
    darcs get --partial http://darcs.haskell.org/packages/containers
</code>

6. Potential gotchas

  JHC doesn't have a heap of mac users, so there were a few problems I had in installing.

  in ./src/data/rts/jhc_rts_header.h:

<code>
    -#include <endian.h>
    +#include <sys/types.h>
    +#include <sys/param.h>
</code>

  make libs doesn't always seem to work off the bat. so long as jhc builds, it's probably ok
  for the moment - copy the jhc binary in the root jhc directory to somewhere in your $PATH.

## Contributors

* Mark Wotton
* James Britt
* Josh Price

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
[jhc]: http://repetae.net/computer/jhc/
