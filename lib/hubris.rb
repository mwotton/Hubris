require 'dl/import'
require 'tempfile'
require 'rubygems'
require 'open4'

class Hubris
  VERSION = '0.0.2'
#  class Hubris
    if RUBY_VERSION =~ /^1\.8/
      extend DL::Importable
    else
      extend DL::Importer
    end
    
    def initialize(haskell_str)
      build_jhc(haskell_str)
    end
    
    def noisy(str)
      pid, stdin, stdout, stderr = Open4::popen4(str)
      ignored, status = Process::waitpid2 pid
      if status != 0 then
        msg = <<"EOF"
output: #{stdout.read}
error:  #{stderr.read}
EOF
        return [false,str + "\n" + msg]
      else
        return [true,str + "\n"]
      end
    end
    
    def build_jhc(haskell_str)
      system("rm hs.out_code.c")
      file=Tempfile.new("TempHs.hs")
      # cheap way: assert type sigs binding to RValue. Might be able to do better after,
      # but this'll do for the moment
      file.print(<<EOF
{-# LANGUAGE FlexibleInstances, ForeignFunctionInterface, UndecidableInstances #-}
import Foreign.Ptr
import RubyMap

main :: IO ()
main = return ()
EOF
                 )
      file.print(haskell_str)
      puts("Hask: #{haskell_str}\n")
      # TODO add foreign export calls immediately for each toplevel func
      # cheap hacky way: first word on each line, nub it to get rid of
      # function types.
      # tricky bit: generating interface for each
      functions={}
      haskell_str.each_line do |line|
        # skkeeeeeeetchy. FIXME use haskell-src-exts or something more sensible here
        if line =~ /^[^ \-{].*/ then
          functions[line.split(/ /)[0]]=1   
        end
      end
      if functions.size() == 0
        # no point going on, there's nothing to load
        return
      end

      functions.keys.each do |fname|
        file.print <<"EOF"

#{fname} :: RValue -> RValue
#{fname}_external :: Value -> Value
#{fname}_external x = toRuby $ #{fname} $ fromRuby x
foreign export ccall "#{fname}_external" #{fname}_external :: Value -> Value

EOF
      end
      
      file.flush
      # this is so dumb. Go delete the file when we're done
      # debugging
      system("cp #{file.path} #{file.path}.hs")
      success, msg = noisy("jhc -dc #{file.path}.hs -ilib")
      if not (success || File.exists?("hs.out_code.c")) then
        file.rewind
        raise SyntaxError, "JHC build failed:\nsource\n#{file.read}\n#{msg}"
      end
      # output goes to hs_out.code.c
      # don't need to grep out main any more
      # FIXME unique name for dynamic lib
      libname = "lib_#{rand().to_s.slice(2,10)}.dylib"
    
      success,msg = noisy("gcc -c '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' '-D_JHC_STANDALONE=0' -ffast-math -Wshadow -Wextra\
 -Wall -Wno-unused-parameter -DNDEBUG -O3 -fPIC -shared ./hs.out_code.c ./lib/rshim.o -I/opt/local/lib/ruby/1.8/i686-darwin9/ -I./lib -o #{libname}")
      if not success then
        raise SyntaxError, "C build failed:\n#{msg}"
      end
      Hubris.dlload libname
      # get all the headers from ... somewhere
      headers = []
      headers.each do |header|
        extern header
      end
      extern "hs_init"
      hs_init
      # TODO load all the object headers into the lib
    end
 
# end
  
end
