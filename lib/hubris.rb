require 'tmpdir'
require 'rubygems'
require 'open4'
require 'digest/md5'
require 'rbconfig'
$:.unshift File.dirname(__FILE__) + "/../ext/stub"
require 'stub'
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
  HUBRIS_DIR = ENV['HUBRIS_DIR'] || "/var/hubris"
  SO_CACHE = File.expand_path(HUBRIS_DIR + "/cache")
  HS_CACHE = File.expand_path(HUBRIS_DIR + "/source")
  # require 'stub'
  [SO_CACHE,HS_CACHE].each {|dir| FileUtils.mkdir_p(dir)}
  $:.push(SO_CACHE)
  @always_rebuild=false

  @@basepackages = []
  def self.add_packages(packages)
    @@basepackages.concat packages
  end

  # load the new functions into target
  def hubris(options = { })
    # :inline beats :source beats :mod
    immediate, source, mod,packages = [:inline,:source,:module,:packages].collect {|x| options[x]}
    build_args = options[:no_strict] ? [] : ["--strict"]
    # puts packages

    # leaves generated source lying around - might want to clean up, but
    # useful for debugging too...
    source = anonymous_module(immediate, build_args) if immediate
    mod    = gen_modname(source)                     if source
    raise "code error, should never happen" unless mod
    require(hubrify(mod,build_args,source||"",packages||[])) # let it crash

    # bind the functions from the Exports namespace
    include(eval("Hubris::Exports::#{zencode(mod)}"))
  end

  private

  def anonymous_module(source,build_args)
    mod = "Inline_#{Digest::MD5.hexdigest(source + build_args.to_s)}"
    filename = "#{HS_CACHE}/#{zencode(mod)}.hs"
    File.open(filename, "w") {|h| h.write "module #{mod} where\nimport Language.Ruby.Hubris.Binding\n#{source}\n"}
    return filename
  end

  def gen_modname(filename)
    # find the code, compile into haskell module in namespace, set
    l = File.open(filename).read
    l =~ /^ *module *([^ \t\n]*)/
    return $1
  end

  def zencode(name)
    name.gsub(/Z/, 'ZZ').gsub(/z/, 'zz').gsub(/\./,'zi').gsub(/_/,'zu').gsub(/'/, 'zq')
  end

  def hubrify(mod, args, src,packages=[])
    libFile = "#{SO_CACHE}/#{zencode(mod)}.#{dylib_suffix}"
    headers = ""
    libraries = ""
    if @always_rebuild or !File.exists?(libFile)
      status,msg = Hubris.noisy("Hubrify #{headers} #{libraries} -v --module #{mod} --output #{libFile} #{args.join(' ')} " +
                                (packages+@@basepackages).collect{|x| "--package #{x}"}.join(' ') + ' ' + src)
      # if Hubrify's not installed, we throw an exception. just as
      # good as explicitly checking a flag.
      # puts msg
      raise HaskellError.new("Hubrify error:\n#{msg + status.exitstatus.to_s}") unless status.exitstatus == 0
    end
    return libFile
  end

  def dylib_suffix
    case Config::CONFIG['target_os']
    when /darwin/; "bundle"
    when /linux/;  "so"
    else;          "so" #take a punt
    end
  end

  def self.noisy(str)
    pid, stdin, stdout, stderr = Open4.popen4 str
    # puts "running #{str}\n"


    # puts "Status: #{status.exitstatus}"
    # puts "#{pid} done"


    msg =<<-"EOF"
ran   |#{str}|
output|#{stdout.read}|
error |#{stderr.read}|
EOF
    puts msg
    ignored, status = Process.waitpid2 pid
    msg += "status |#{status}|"
    return status, msg
  end

end

# this may be sketchy :)
class Class
  include Hubris
end

#    rescue LoadError
#      raise HaskellError, "loading #{libFile} failed:\n#{$!.to_s}" +
#        `nm #{libFile} 2>/dev/null` + "\n"
#    end
