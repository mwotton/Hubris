require 'tmpdir'
require 'rubygems'
require 'open4'
require 'digest/md5'
require 'rbconfig'

# require 'file/temp'
# require 'libHaskell'
# TODO delete old files

class HaskellError < RuntimeError
end

class File
  # takes block
  @@used = 0
  def self.withTempFile(template)
    # put the code in a temporary file, set opt[:source]
    
    filename="/tmp/foo_#{$$}_#{@@used}.hs" # File::Temp.new(false)
    @@used += 1
    handle=File.open(filename, 'w')
    yield(filename, handle)
    File::delete(filename)
  end
end

module Hubris
  VERSION = '0.0.2'
  SO_CACHE = File.expand_path("~/.hubris_cache")
  require 'HubrisStubLoader'
  system('mkdir ' + SO_CACHE)
  $:.push(SO_CACHE)

  @packages = []
  def self.add_packages(packages)
    @packages.concat packages
  end
  
  # load the new functions into target
  def hubris(options = { })
    build_args = options[:no_strict] ? [""] : ["--strict"] # FIXME

    if options.keys.select{ |x| x==:source || x==:module || x==:inline }.count != 1
      raise "Bad call - needs exactly one of :source, :module or :inline defined in hubris call"
    end
    if    options[:inline]
      # FIXME, how do we come up with a random module name
      
      File.withTempFile("xxxx.hs") do |filename, handle|
        filename =~ /([^\/]*)\.hs$/
        mod = $1.capitalize
        handle.write "module #{mod} where\n" + options[:inline]+"\n"
        handle.close
        build(filename, build_args)
      end
    elsif options[:source]
      build(options[:source], build_args)
    elsif options[:module]
      load(options[:module])
    else
      raise "code error, should never happen"
    end
  end

  def self.trans_name(func)
    func.gsub(/Z/, 'ZZ').gsub(/z/, 'zz').gsub(/\./,'zd').gsub(/_/,'zu').gsub(/'/, 'zq') 
  end
  
  def build(source, args)
    # find the code, compile into haskell module in namespace, set 
    l = File.open(source).read
    l =~ /^ *module *([^ \t\n]*)/
    mod = $1
    print "source: #{source}\n"
    print "module name is #{mod}\n"
    libFile = genLibFileName(mod)
    status,msg = noisy("Hubrify #{mod} #{source} #{args.join(' ')}")
    if not status
      raise HaskellError.new("Couldn't compile the module, FIXME:\n#{msg}")
    else
      puts "Succeeded: #{msg}"
    end
#     # this needs fixing
#     status,msg= noisy("cc -dynamic -arch i386 -bundle -dynamic -flat_namespace -undefined suppress -weak_reference_mismatches non-weak -undefined suppress -o #{libFile} -lbundle1.o -L. -L/opt/local/lib -L.  -L/usr/local/lib -L/usr/lib/i686-apple-darwin9/4.0.1 -L/usr/lib/gcc/i686-apple-darwin9/4.0.1  #{prelim} Language.Ruby.Hubris.Exports.#{mod}.aux.o /usr/local/lib/ghc-6.13.20090928/libHSrts-ghc6.13.20090928.dylib -lruby -lpthread -ldl -lobjc -lgcc_s.10.5 -lgcc -lSystem")

#     if status
#       raise HaskellError.new("Couldn't link the module, FIXME:\n#{msg}")
#     else
#       puts "Succeeded: #{msg}"
#     end
    load(mod)
  end
  
  def load(mod)
    # search path for modules?
    libFile = genLibFileName(mod)
    begin
      puts "requiring #{libFile}"
      require libFile
      puts "reqd"
      # raise LoadError
    rescue LoadError
      raise HaskellError, "loading #{libFile} failed: " +
        "\n" + $!.to_s + "\n" + `nm #{libFile}` + "\n"
    end
    # bind them all into target_module
    include(eval("Hubris::Exports::" + Hubris.trans_name(mod)))
  end
  private
  
  def genLibFileName(mod)
    cache = "/var/hubris/cache"
    return "#{cache}/#{Hubris.trans_name(mod)}.#{dylib_suffix}"
  end
  
  def dylib_suffix
    case Config::CONFIG['target_os']
    when /darwin/
      "bundle"
    when /linux/
      "so"
    else
      "so" #take a punt
    end
  end
  def noisy(str)
    pid, stdin, stdout, stderr = Open4.popen4 str
    # puts "waiting for #{pid}, running #{str}"

    ignored, status = Process.waitpid2 pid
    puts "Status: #{status}"
    # puts "#{pid} done"
    
    
    msg =<<-"EOF"
ran   |#{str}|
output|#{stdout.read}|
error |#{stderr.read}|
EOF
    return [status, msg]
  end
  
end
# this may be sketchy :)
class Class
  include Hubris
  #   self.class_eval do
  #     def hubris(options)
  #       Hubris.hubris(self, options)
  #     end
  #   end
end

##### DEPRECATED #########

module HubrisOld
  private
  def ruby_header_dir
    # Possible config values for 1.8.6:
    # archdir and topdir
    # For 1.9: rubyhdrdir
    Config::CONFIG['rubyhdrdir'] || Config::CONFIG['topdir'] 
  end



  def self.base_lib_dir
    File.expand_path( File.dirname(__FILE__))
  end


  def self.find_suitable_ghc()
    # if HUBRIS_GHC is specified, don't try anything else.
    ghcs = ENV['HUBRIS_GHC'] ||  Dir.glob(ENV['PATH'].split(':').map {|p| p + "/ghc*" }).select {|x| x =~ /\/ghc(-[0-9\.]*)?$/}
    ghcs = ghcs.each { |candidate|
      version = `#{candidate} --numeric-version`.chomp
      return [candidate, version] if version >= '6.11' 
    }
    raise(HaskellError, "Can't find an appropriate ghc: tried #{ghcs}")
  end

  # GHC,GHC_VERSION = Hubris::find_suitable_ghc
  # RubyHeader = ruby_header_dir or raise HaskellError, "Can't get rubyhdrdir"



  def self.base_loader_code mod_name, lib_name
    %~/* so, here's the story. We have the functions, and we need to expose them to Ruby */
/* this is about as filthy as it looks, but gcc chokes otherwise, with a redefinition error. */
#define HAVE_STRUCT_TIMESPEC 1 
#include <stdio.h>
#include "ruby.h"
VALUE #{mod_name} = Qnil;
extern void hs_init(int * argc, char ** argv[]);

void Init_#{lib_name}() {
    int argc = 1;
    // this needs to be allocated on the heap or we get a segfault
    char ** argv = malloc(sizeof(char**) * 1);
    argv[0]="haskell_extension";
//    printf("initialising #{lib_name}\\n");

    hs_init(&argc, &argv);
   // printf("initialised #{lib_name}\\n");
    #{mod_name} = rb_define_class("#{mod_name}", rb_cObject);
   // printf("defined classes for #{lib_name}\\n");
    ~

      # class Module; def hubris; self.class_eval { def self.h;"hubrified!";end };end;end
  end

  def self.make_stub(mod_name, lib_name, functions)
    loader_code = base_loader_code(mod_name, lib_name)

    functions.each do |function_name|
      loader_code += "VALUE #{function_name}_external(VALUE);\n"
      # FIXME add the stg roots as well
      #  loaderCode += "extern void __stginit_#{function_name}zuexternal(void);\n"
    end

    functions.each do |function_name|
      # FIXME this is the worng place to be binding methods. Can we bind a bare C method in Ruby
      # instead?
      #      loader_code += "rb_define_method(#{mod_name},\"#{function_name}\",#{function_name}_external, 1);\n"
      loader_code += "rb_define_singleton_method(#{mod_name},\"#{function_name}\",#{function_name}_external, 1);\n"
      # FIXME this is needed for GHC
      # loader_code += "hs_add_root(__stginit_#{trans_name(function_name + '_external')});\n"
    end
    return loader_code + "}\n"
  end

  def write_hs_file file_path, haskell_str, functions, mod_name, lib_name
    File.open( file_path , "w") do |file|
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
    end
  end

end


