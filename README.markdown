# Hubris

## Description

This is a quick and dirty way to call Haskell functions from ruby.

Hubris will wash your car, lie to your boss, and salvage your love life.
If you are very, very lucky, it might also let you get some uber-fast
functional goodness into your ruby programs through the back door.

## Synopsis

UPDATE: so it now works, for fixed length integers, sort of. (What a ringing endorsement, hey?)

anyway, as you can see in the spec, you can use it a little like this:

<pre>
class Target
  include Hubris
  def negate(i)
    return -i
  end
end

t.inline "mydouble (T_FIXNUM i) = T_FIXNUM (i + i)"
t.inline "mytriple (T_FIXNUM i) = T_FIXNUM (i * 3)"
puts t.negate(3)
puts t.mydouble(i)
puts t.mytriple(i)
</pre>

This code is a bit of a black triangle <http://rampantgames.com/blog/2004/10/black-triangle.html>
It doesn't seem like it does much, but here we have inline code generation, some marshalling support, 
and the ability to load multiple haskell chunks without stomping on each other or the original methods.

next will be support for the other Ruby data types, GHC support, and some caching so it doesn't recompile every
time you load the file unless the haskell code has actually changed.

as before, if all else fails, mail mwotton@gmail.com with tales of woe.

## Requirements

* haskell platform (or ghc-6.8.2 or better and c2hs)
* jhc (John's Haskell Compiler)
* gcc (oh, come on. don't tell me you don't have it)
* ruby 1.8.6 or higher
* Mac OSX or Linux
* bash

## Install

- Install the Hubris gem from RubyForge

<pre>
    sudo gem install hubris
</pre>

- Or live on the bleeding edge and install the latest from Github

<pre>
    gem source --add http://gems.github.com
    sudo gem install mwotton-hubris
</pre>

- Get the [Haskell Platform][haskell_platform] and install this for your platform (now we have GHC for building JHC)
- you may have to run "sudo cabal install binary zlib utf8-string readline fgl" - am not entirely sure what's in the Haskell
  Platform. Feedback on this step appreciated.
- JHC is thankfully now a little easier to build. Follow the instructions at http://repetae.net/computer/jhc/building.shtml - the
  most recent tested working version is http://repetae.net/dist/jhc-0.7.1.tar.gz.
<pre>
    ./configure && make && sudo make install
</pre>

should do the trick.

- this will copy the jhc binary to /usr/local/bin. If it's not in your
  PATH already, add "export PATH=$PATH:/usr/local/bin" to your .bashrc.

to create RubyMap.hs:
  c2hs -v --cppopts='-I/opt/local/include/ruby-1.9.1/ruby' --cpp=gcc --cppopts=-E --cppopts=-xc RubyMap.chs  

FIXME this is not a gem of lucid clarity right now

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
