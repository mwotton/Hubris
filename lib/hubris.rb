require 'dl/import'
require 'tempfile'

module Hubris
  VERSION = '0.0.1'
  class Hubris
    extend DL::Importer # Importable in 1.8, FIXME
    
    def initialize(haskell_str)
      build_jhc(haskell_str)
    end
    
    def noisy(str)
      print "Noisy: #{str}\n"
      system(str)
    end
    
    def build_jhc(haskell_str)
      file=Tempfile.new("TempHs.hs")
      # cheap way: assert type sigs binding to RValue. Might be able to do better after,
      # but this'll do for the moment
      file.print(<<EOF
import RubyMap
import Mapper
main :: IO ()
main = return ()
EOF
                 )
      file.print(haskell_str)
      # TODO add foreign export calls immediately for each toplevel func
      # cheap hacky way: first word on each line, nub it to get rid of
      # function types.
      # tricky bit: generating interface for each
      functions={}
      haskell_str.each_line do |line|
        # if line ~= /^[^ ]/ then
        functions[line.split(/ /)[0]]=1
      end
      functions.keys.each do |fname|
        file.print "#{fname} :: RValue -> IO RValue\n"
        # end
      end
      
      file.flush
      # this is so dumb
      system("cat #{file.path}; cp #{file.path} #{file.path}.hs")
      if(0!=noisy("jhc  #{file.path}.hs -ilib")) then
          raise SyntaxError, "JHC build failed"
      end
      # output goes to hs_out.code.c
      # don't need to grep out main any more
      # FIXME unique name for dynamic lib
      lib = Tempfile.new("libDyn.so")
      if(0!=noisy("gcc '-std=gnu99' -D_GNU_SOURCE -D'-falign-functions=4' '-D_JHC_STANDALONE=0' -ffast-math -Wshadow -Wextra -Wall -Wno-unused-parameter -o libdynhs.so \
 -DNDEBUG -D_JHC_STANDALONE=0 -O3 -fPIC -shared #{file.dirname}/hs.out_code.c -o {lib.name}")) then
          raise SyntaxError, "C build failed"
      end
      dlload lib.name
      # get all the headers from ... somewhere
      headers = []
      headers.each do |header|
        extern header
      end
      extern "hs_init"
      hs_init
      # TODO load all the object headers into the lib
    end
  end
  
end
