require 'tmpdir'
require 'rubygems'
require 'open4'
require 'digest/md5'
# TODO delete old files

module Hubris
  VERSION = '0.0.2'
  GHC_VERSION='20090903'
  GHC ='/usr/local/bin/ghc-6.11.' + GHC_VERSION 

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
{-# LANGUAGE FlexibleInstances, ForeignFunctionInterface, UndecidableInstances #-}
import Foreign.Ptr()
import RubyMap
import Control.Exception()

main :: IO ()
main = return ()
EOF
    bindings = ""
    # cheap way: assert type sigs binding to RValue. Might be able to do better after,
    # but this'll do for the moment
    functions.each do |fname|
      bindings +=<<-"EOF"
#{fname} :: RValue -> RValue
#{fname}_external :: Value -> Value -> Value
#{fname}_external _mod x = toRuby $ #{fname} $ fromRuby x
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
#include <ruby.h>
VALUE #{modName} = Qnil;
extern void hs_init(int * argc, char ** argv[]);

void Init_#{libName}() {
    int argc = 1;
    char ** argv = malloc(sizeof(char**) * 1);
    argv[0]="my_proggy";
    // fixme why does this crash 
//    printf("Yay, we've called the init function\\n");
    hs_init(&argc, &argv); // significant difference between a null pointer and a pointer to a null pointer
  //  printf("Yay, we've finished calling the init function\\n");
    // allegedly this works for pre-existing classes as well
    #{modName} = rb_define_class("#{modName}", rb_cObject);
    EOF
    
    functions.each do |functionName|
      loaderCode += "VALUE #{functionName}_external(VALUE);\n"
    #  loaderCode += "extern void __stginit_#{functionName}zuexternal(void);\n"
    end

  
    functions.each do |functionName|
      loaderCode += "rb_define_method(#{modName},\"#{functionName}\",#{functionName}_external, 1);\n"
      # FIXME this is needed for GHC
      # FIXME this will break for anything with Z in it :)
      # loaderCode += "hs_add_root(__stginit_#{trans_name(functionName + '_external')});\n"
    end
    return loaderCode + "}"
  end
  
  def inline(haskell_str)
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
    
    builders = { "jhc" => lambda { |x,y,z| jhcbuild(x,y,z) },
                 "ghc" => lambda { |x,y,z| ghcbuild(x,y,z) } }

     
 
    signature = Digest::MD5.hexdigest(haskell_str)
    functions = extract_function_names(haskell_str)
    unless functions.size > 0
      return
    end
    `rm lib*.so`
    libName = "lib#{functions[0]}_#{signature}"; # unique signature
    libFile = "#{libName}.so"
    file = File.new(File.join(Dir.tmpdir, functions[0] + "_source.hs"), "w")
    if false # File.exists?(libFile)
      # puts "Yay, no recompilation"
    else
      # so the hashing algorithm doesn't collide if we try building the same code
      # with jhc and ghc.
      file.print("-- COMPILED WITH #{builder}\n")
      file.print(make_haskell_bindings(functions))
      file.print(haskell_str)
      file.flush

      modName = self.class  
      File.open("stubs.c", "w") {|io| io.write(make_stub(modName,libName, functions))}
      # and it all comes together
      
      build_result = builders[builder].call(libFile, file.path, ['stubs.c','./lib/rshim.c'])
      # File.delete(file.path)    
    end
    # puts "about to load #{libName}"
    begin
      require libName
      # puts "loaded #{libName}"
    rescue LoadError
      raise LoadError,       "loading #{libName} failed, source was\n" + `cat #{file.path}` + "\n" + $!.to_s + "\n" + `nm #{libName}.so |grep ext` + "\n" + (build_result || "no build result?")
    end
  end
  
  def ghcbuild(libFile, haskell_path, extra_c_src)
    success,msg=noisy("#{GHC} -Wall --make -dynamic -fPIC -shared #{haskell_path} -lHSrts-ghc6.11.#{GHC_VERSION} \
-L/usr/local/lib/ghc-6.11.#{GHC_VERSION} -no-hs-main -optl-Wl,-rpath,/usr/local/lib/ghc-6.11.#{GHC_VERSION} -o #{libFile} " + 
                      extra_c_src.join(' ') + ' ./lib/RubyMap.hs -I/usr/local/include/ruby-1.9.1/ -I./lib')
    # puts [success,msg]
    unless success
      raise SyntaxError, "ghc build failed " + msg + `cat #{haskell_path}`
    end
    return msg
  end

  def jhcbuild(libFile, haskell_path, extra_c_src)
    noisy("rm hs.out_code.c 2>/dev/null")
    # puts "building\n#{file.read}"
    success, msg = noisy("jhc  -dc #{haskell_path} -papplicative -ilib")
    unless success || File.exists?("hs.out_code.c")
      raise SyntaxError, "JHC build failed:\nsource\n" + `cat #{haskell_path}` + "\n#{msg}"
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
