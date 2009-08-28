require 'dl/import'
require 'tempfile'
require 'rubygems'
require 'open4'
# require 'ffi'

module Hubris
  VERSION = '0.0.2'

  # this is probably a horrible place to put it
  def Hubris.init_with_dl(l,modname)
    extend DL::Importer
    dlload l
    #require modname
    
#     headers.each do |f|
#       # argh, none of the tools know about the C-side representation of a ruby value.
#       # just call it a 
#       extern " #{f}(VALUE)"
#     end
    return FIXME
  end
  
  class Payload
    def initialize(l, modname)
      Hubris.init_with_dl(l,headers)
    end

    def init_with_FFI(l,headers)
      include FFI::Library
      # hm, how do we say we want the libname library??
      libname=l.gsub /\.dylib/, ''
      puts "Loading #{libname}"
      ffi_lib libname
      # get all the headers from ... somewhere
      # attach_function :hs_init, [:pointer, :pointer], :void
      headers.each do |header|
        puts "Header is #{header}"
        attach_function header, [:object ], :object
      end
      # extern "hs_init"
      
      # TODO load all the object headers into the lib
    end
  end
  #  class Hubris
  #     if RUBY_VERSION =~ /^1\.8/
  #       extend DL::Importable
  #     else
  #       extend DL::Importer
  #     end
  def Hubris.build(haskell_str) 
    return JHC_builder.new(haskell_str)
  end
  
  class JHC_builder
    include Hubris
    def initialize(haskell_str)
      system("rm hs.out_code.c")
      file=Tempfile.new("TempHs.hs")
      # FIXME unique name for dynamic lib
      # libname = "libfoo_#{rand().to_s.slice(2,10)}"
      libname = "libfoo"
      libfile = libname + ".bundle"
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
      modName = "FIXME"
      loaderCode =<<"EOF"
/* so, here's the story. We have the functions, and we need to expose them to Ruby */

VALUE #{modName} = Qnil;
void Init_#{libname}() {
  // maybe init haskell side stuff later
    #{modName} = rb_define_module("#{modName}");
EOF

      functions.keys.each do |functionName| 
        loaderCode += "rb_define_method(#{modName},\"#{functionName}\",#{functionName}_external, 1);"
      end
      loaderCode += "}"

      # output goes to hs_out.code.c
      # don't need to grep out main any more
      # we do need to grep out rshim.h, though. why? no one knows. better solution please
      # also chucking the loader code in there.
      system("echo '#include <rshim.h>' > temp.c;");
      system("grep -v '#include \<rshim.h\>' < hs.out_code.c | sed  's/ALIGN(/JHCS_ALIGN(/g'  >> temp.c; mv temp.c hs.out_code.c;");
      f = File.open("hs.out_code.c", "a")
                  
      f.write(loaderCode)
      f.close
      
      lDFLAGS = [ '-dynamiclib','-fPIC', '-shared', '-lruby']
      cPPFLAGS = ['-D_GNU_SOURCE', '-D_JHC_STANDALONE=0', '-DNDEBUG' ]
      cFLAGS = ['-std=gnu99', 
                '-falign-functions=4', 
                '-ffast-math', 
                '-Wshadow', '-Wextra', '-Wall', '-Wno-unused-parameter',
                '-O3 -o libfoo.bundle']
      sRC = ['./hs.out_code.c', './lib/rshim.c']
      
      iNCLUDES = ['-I/opt/local/include/ruby-1.9.1/', '-I./lib']

      system 'rm "libfoo.bundle"'
      
      success,msg = noisy("gcc " + [cPPFLAGS, cFLAGS, lDFLAGS, iNCLUDES, sRC].join(" "))
      # FIXME better handling of ruby includes
#       success,msg = noisy("gcc  '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4'
#  '-D_JHC_STANDALONE=0' -ffast-math -Wshadow -Wextra\
#  -Wall -Wno-unused-parameter -dynamiclib -DNDEBUG -O3 -fPIC -shared ./hs.out_code.c ./lib/rshim.c -lruby -I/opt/local/include/ruby-1.9.1/ -I./lib -o libfoo.bundle")
      if not success then
        raise SyntaxError, "C build failed:\n#{msg}"
      end
      require "libfoo"
      return FIXME
    end
    

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
  
  # end
  
end
