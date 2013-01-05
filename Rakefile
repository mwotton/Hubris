
include Rake::DSL
require 'bundler'
Bundler::GemHelper.install_tasks
require 'rake'
require 'fileutils'

# require 'rake-compiler'
require 'rake/extensiontask'
# require 'rake/extensiontesttask'


# task "build:native" => [:no_extconf, :native, :build] do
#     file = "pkg/stub-#{`cat VERSION`.chomp}.gem"
#     mv file, "#{file.ext}-i686-linux.gem"
# end


Rake::ExtensionTask.new('stub')

# intended to be called by the gem builder
task :haskell_compile => [:compile] do
  ghc_version='/usr/bin/ghc' # FIXME, should be able to pick
  # this out from somewhere
  # write the Includes file
  pwd =`pwd`.strip
  arch_headers = "#{RbConfig::CONFIG['rubyhdrdir']}/#{RbConfig::CONFIG['arch']}"
  lib_dir = RbConfig::CONFIG['libdir']
  headers = RbConfig::CONFIG['rubyhdrdir']
  File.open("#{pwd}/Haskell/Language/Ruby/Hubris/Includes.hs","w") do |file|
    file.write "module Language.Ruby.Hubris.Includes where
extraIncludeDirs = [\"#{headers}\", \"#{arch_headers}\"]"
  end
#  command="cd Haskell; cabal update; cabal install
  #  --extra-include-dirs=#{RbConfig::CONFIG['rubyhdrdir']}
  #  --extra-include-dirs=#{RbConfig::CONFIG['rubyhdrdir']}/#{RbConfig::CONFIG['arch']} --extra-lib-dirs=#{RbConfig::CONFIG['libdir']} --user  --enable-shared  --with-ghc=#{ghc_version}"
  command="cabal update; cabal install zlib --enable-shared; cd Haskell; cabal install --extra-include-dirs=#{arch_headers} --extra-include-dirs=#{headers} --extra-lib-dirs=#{lib_dir} --user  --enable-shared  --with-ghc=#{ghc_version}  --verbose --disable-library-profiling"
  puts "running #{command}"
  result=%x{#{command}}
end

task :no_extconf do
    $gemspec.extensions = []
end

task :default => :haskell_compile

task :clean do
  FileList[File.expand_path("~/.hubris_cache/*"),
           'lib*.so', 'lib/*.o' ].each do |f|
    File.delete(f) rescue nil
  end
end

