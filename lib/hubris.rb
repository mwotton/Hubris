require 'dl/import'
require 'tempfile'

module Hubris
  VERSION = '0.0.1'
  class Hubris
    extend DL::Importer # Importable in 1.8, FIXME
    
    def initialize(haskell_str)
      build_jhc(haskell_str)
    end
    
    
    def build_jhc(haskell_str)
      file=Tempfile.new("TempHs.hs")
      # TODO add foreign export calls immediately for each toplevel func
      # cheap hacky way: first word on each line, nub it to get rid of
      # function types.
      # tricky bit: generating interface for each
      headers = haskell_str.each_line do |line|
        file.write "#{line.split(/ /)[0]} :: RValue -> IO RValue"
      end
      # cheap way: assert type sigs binding to RValue. Might be able to do better after,
      # but this'll do for the moment
      file.write(<<EOF
main :: IO ()
main = return ()
EOF
                 )
      file.write(haskell_str)
      if(!system("jhc --no-main #{file.path}")) then
          raise SyntaxError, "JHC build failed"
      end
      # output goes to hs_out.code.c
      # don't need to grep out main any more
      # FIXME unique name for dynamic lib
      lib = Tempfile.new("libDyn.so")
      if(!system("gcc '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wshadow -Wextra -Wall -Wno-unused-parameter -o libdynhs.so \
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
