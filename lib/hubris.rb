require 'tmpdir'
require 'rubygems'
require 'open4'
require 'digest/md5'
require 'rbconfig'
# TODO delete old files

class HaskellError < RuntimeError
end

module Hubris
  VERSION = '0.0.2'
  SO_CACHE = File.expand_path("~/.hubris_cache")

  system('mkdir ' + SO_CACHE)
  $:.push(SO_CACHE)
  # more grungy shell hacking to find an appropriate GHC
  # arguably should be done at install...
  ghc_cmd =<<'EOF'
find $(echo $PATH | sed -e 's/:/ /g') -regex '.*/ghc\(\-[0-9\.]*\)'
EOF
  res = `#{ghc_cmd}`
  #puts res
  ghcs = res.split.select { |candidate|
    # puts candidate
    `#{candidate} --version | sed 's/^.*version *//'` >= '6.11' # yay, fragile
  }

  if ghcs.empty?
    raise HaskellError, "Can't find an appropriate ghc"
  end
  
  #otherwise take the first
  GHC = ghcs[0]
  GHC =~ /ghc-(.*)/ # will fail horribly for plain ghc
  GHC_VERSION = $1
  RubyHeader = Config::CONFIG['rubyhdrdir'] or
    raise HaskellError, "Can't get rubyhdrdir"

  # TODO add foreign export calls immediately for each toplevel func
  # cheap hacky way: first word on each line, nub it to get rid of
  # function types.
  # tricky bit: generating interface for each
  def extract_function_names(haskell_str)
    functions = {}
    haskell_str.each_line do |line|
      # skkeeeeeeetchy. FIXME use haskell-src-exts or something more sensible here
      if /^[^ \-{].*/ =~ line
        functions[line.split(/ /)[0]] = 1
      end
    end    
    functions.keys
  end
  
  def make_haskell_bindings(functions)
    prelude =<<-EOF
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, ForeignFunctionInterface, UndecidableInstances #-}
-- import Foreign.Ptr()
import RubyMap
import Prelude hiding (catch)
import Control.Exception(SomeException, evaluate, catch)
import Foreign(unsafePerformIO)
main :: IO ()
main = return ()

EOF
    bindings = ""
    # cheap way: assert type sigs binding to RValue. Might be able to do better after,
    # but this'll do for the moment
    functions.each do |fname|
      bindings +=<<-EOF
#{fname} :: RValue -> RValue
#{fname}_external :: Value -> Value -> Value
#{fname}_external _mod x = unsafePerformIO $
  (evaluate (toRuby $ #{fname} $ fromRuby x))
     `catch` (\\y -> throwException (show (y::SomeException)))
foreign export ccall "#{fname}_external" #{fname}_external :: Value -> Value -> Value

      EOF
    end
    return prelude + bindings
  end
  
  def trans_name(func)
    func.gsub(/Z/, 'ZZ').gsub(/z/, 'zz').gsub(/\./,'zd').gsub(/_/,'zu').gsub(/'/, 'zq') 
  end


  def make_stub(modName, libName, functions)

    loaderCode =<<-"EOF"
/* so, here's the story. We have the functions, and we need to expose them to Ruby */
#include "rshim.h"
VALUE #{modName} = Qnil;
extern void hs_init(int * argc, char ** argv[]);

void Init_#{libName}() {
    int argc = 1;
    // this needs to be allocated on the heap or we get a segfault
    char ** argv = malloc(sizeof(char**) * 1);
    argv[0]="haskell_extension";
    hs_init(&argc, &argv);
    #{modName} = rb_define_class("#{modName}", rb_cObject);
    EOF
    
    functions.each do |functionName|
      loaderCode += "VALUE #{functionName}_external(VALUE);\n"
      # FIXME add the stg roots as well
      #  loaderCode += "extern void __stginit_#{functionName}zuexternal(void);\n"
    end

  
    functions.each do |functionName|
      # FIXME this is the worng place to be binding methods. Can we bind a bare C method in Ruby
      # instead?
      loaderCode += "rb_define_method(#{modName},\"#{functionName}\",#{functionName}_external, 1);\n"
      # FIXME this is needed for GHC
      # loaderCode += "hs_add_root(__stginit_#{trans_name(functionName + '_external')});\n"
    end
    return loaderCode + "}\n"
  end
  
  def inline(haskell_str, build_options={ })
    builder = "ghc"
    # this is a bit crap. You wouldn't have to specify the args in an FP language :/
    # should probably switch out to a couple of single-method classes
    # argh
    # """
    # Ruby's lambda is unusual in that choice of parameter names does affect behavior:
    # x = 3
    # lambda{|x| "x still refers to the outer variable"}.call(4)
    # puts x  # x is now 4, not 3
    # """
    # this is a solved problem, guys. come ON. FIXME
    
    builders = { "jhc" => lambda { |x,y,z,a| jhcbuild(x,y,z,a) },
                 "ghc" => lambda { |x,y,z,a| ghcbuild(x,y,z,a) } }
 
    signature = Digest::MD5.hexdigest(haskell_str)
    functions = extract_function_names(haskell_str)
    unless functions.size > 0
      return
    end
    libName = "lib#{functions[0]}_#{signature}"; # unique signature
    
    dylib_suffix = case Config::CONFIG['target_os']
                   when /darwin/
                     "bundle"
                   when /linux/
                     "so"
                   else
                     "so" #take a punt
                   end
    libFile = SO_CACHE + "/" + libName + '.' + dylib_suffix
                                                   
    
    file = File.new(File.join(Dir.tmpdir, functions[0] + "_source.hs"), "w")
    # if the haskell libraries have changed out from under us, that's just too bad.
    # If we've changed details of this script, however, we probably want to rebuild,
    # just to be safe.
    if !File.exists?(libFile) or File.mtime(__FILE__) >= File.mtime(libFile)
      # so the hashing algorithm doesn't collide if we try building the same code
      # with jhc and ghc.
      #
      # argh, this isn't quite right. If we inline the same code but on a new ruby module
      # this won't create the new stubs. We want to be able to use new stubs but with the
      # old haskell lib. FIXME
      file.print("-- COMPILED WITH #{builder}\n")
      file.print(make_haskell_bindings(functions))
      file.print(haskell_str)
      file.flush

      modName = self.class  
      File.open("stubs.c", "w") {|io| io.write(make_stub(modName,libName, functions))}
      # and it all comes together
      
      build_result = builders[builder].call(libFile, file.path, ['stubs.c','./lib/rshim.c'], build_options)
      # File.delete(file.path)    
    end
    begin
      require libName
      # raise LoadError
    rescue LoadError
      raise LoadError, "loading #{libName} failed, source was\n" + `cat #{file.path}` + 
                       "\n" + $!.to_s + "\n" + `nm #{libFile} |grep 'ext'` + "\n" + 
                       (build_result || "no build result?") + "\n"
    end
  end
  
  def ghcbuild(libFile, haskell_path, extra_c_src, options)
    # this could be even less awful.

    command = "#{GHC} -Wall -v  --make -dynamic -fPIC -shared #{haskell_path} -lHSrts-ghc#{GHC_VERSION} " +
     "-L/usr/local/lib/ghc-#{GHC_VERSION} " +
     "-no-hs-main " +
      #     -L/Users/mwotton/projects/ghc \
      "-optl-Wl,-rpath,/usr/local/lib/ghc-#{GHC_VERSION} " +
      # "-optl-Wl,-macosx_version_min,10.5 " +
    "-o #{libFile} " +  extra_c_src.join(' ') + ' ./lib/RubyMap.hs -I' + Hubris::RubyHeader + ' -I./lib'
    if (not options[:no_strict])
      command += ' -Werror ' # bondage and discipline
    end
    success,msg=noisy(command)
    # puts [success,msg]
    unless success
      raise HaskellError, "ghc build failed " + msg + `cat #{haskell_path}`
    end
    return msg
  end

  def jhcbuild(libFile, haskell_path, extra_c_src)
    noisy("rm hs.out_code.c 2>/dev/null")
    # puts "building\n#{file.read}"
    success, msg = noisy("jhc  -dc #{haskell_path} -papplicative -ilib")
    unless success || File.exists?("hs.out_code.c")
      raise HaskellError, "JHC build failed:\nsource\n" + `cat #{haskell_path}` + "\n#{msg}"
    end
    # puts msg
   
    # output goes to hs_out.code.c
    # don't need to grep out main any more
    # we do need to grep out rshim.h, though. why? no one knows. better solution please
    system("echo '#include <rshim.h>' > temp.c;")
    system("grep -v '#include \<rshim.h\>' < hs.out_code.c | sed  's/ALIGN(/JHCS_ALIGN(/g'  >> temp.c; mv temp.c hs.out_code.c;")

    # FIXME generalise to linux, this is probably Mac only.
    lDFLAGS = [ '-dynamiclib',
                '-fPIC',
                '-shared'
                # '-lruby',
                ]
    mACFLAGS = [
                '-undefined suppress',
                '-flat_namespace'
              ]
    cPPFLAGS = [
                '-D_GNU_SOURCE',
                '-D_JHC_STANDALONE=0',
                '-DNDEBUG'
               ]
    cFLAGS = ['-std=gnu99',
              '-falign-functions=4',
              '-ffast-math',
              '-Wshadow', '-Wextra', '-Wall', '-Wno-unused-parameter',
              "-g -O3 -o #{libFile}"]
    sRC = [
           './hs.out_code.c'
          ] + extra_c_src

    mACiNCLUDES = ['-I/opt/local/include/ruby-1.9.1/', '-I./lib']
    iNCLUDES = ['-I/usr/local/include/ruby-1.9.1/', '-I./lib']

    system "rm #{libFile} 2>/dev/null"

    success, msg = noisy("gcc " + [cPPFLAGS, cFLAGS, lDFLAGS, iNCLUDES, sRC].join(" "))
    puts msg
    unless success
      raise SyntaxError, "C build failed:\n#{msg}"
    end
  end
end

def noisy(str)
  pid, stdin, stdout, stderr = Open4.popen4 str
  ignored, status = Process.waitpid2 pid
  if status == 0
    [true, str + "\n"]
  else
    msg = <<-"EOF"
output: #{stdout.read}
error:  #{stderr.read}
    EOF
    [false, str + "\n" + msg]
  end
end
