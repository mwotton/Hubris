#!/usr/local/bin/ruby

here = File.expand_path( File.dirname(__FILE__))
$:.unshift here + '/../../lib'
require 'hubris'

#Hubris.ruby_header = '/home/james/data/vendor/ruby-1.8.6-p383/'
#Hubris.ghc_cmd = 'ghc'
#Hubris.ghc_version = '6.11.20090913'

class Target
  
  include Hubris
  
  def negate(i)
    return -i
  end
end

t = Target.new

t.inline "mydouble (T_FIXNUM i) = T_FIXNUM (i + i)
my_double _ = T_NIL", :no_strict => true

#t.inline "mytriple (T_FIXNUM i) = T_FIXNUM (i * 3)
# mytriple _ = T_NIL", :no_strict => true
puts t.negate(3)
puts t.mydouble(2)
# puts t.mytriple(8)
