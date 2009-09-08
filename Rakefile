require 'rubygems'
gem 'hoe', '>= 2.1.0'
require 'hoe'
require 'fileutils'
require './lib/hubris'

# Hoe.plugin :newgem
# Hoe.plugin :website
# Hoe.plugin :cucumberfeatures

# Generate all the Rake tasks
# Run 'rake -T' to see list of generated tasks (from gem root directory)
$hoe = Hoe.spec 'hubris' do
  self.developer 'Mark Wotton', 'mwotton@gmail.com'
  self.rubyforge_name = "hubris"
  self.summary = 'tool to help build .so files from haskell code for use in Ruby via dl'
  self.post_install_message = 'PostInstall.txt'
  self.readme_file = "README.markdown"
  self.history_file = "HISTORY.markdown"
end

#require 'newgem/tasks'
Dir['tasks/**/*.rake'].each { |t| load t }

task :spec => "lib/RubyMap.hs"
task "lib/RubyMap.hs" => "lib/RubyMap.chs" do
  # mac
  # system "c2hs -v --cppopts='-I/opt/local/include/ruby-1.9.1/ruby' --cpp=gcc --cppopts=-E --cppopts=-xc lib/RubyMap.chs"
  #linux

  system "c2hs -v --cppopts='-I/usr/local/include/ruby-1.9.1' --cpp=gcc --cppopts=-E --cppopts=-xc lib/RubyMap.chs"
end

task :clean do
  FileList['lib/*.hi', 'lib/*.ho', 'lib/RubyMap.chs.h', 'lib/RubyMap.chi','lib/RubyMap.hs', 
           'hs.out', 'lib/*.o', 'libfoo_*.bundle', 'lib/hs.out_code.c' ].each do |f|
    system "rm #{f}"
  end
  system "cd sample; make clean"

end
