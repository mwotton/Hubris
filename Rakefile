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


file "lib/RubyMap.hs" => ["lib/RubyMap.chs"] do
  str = "c2hs -v --cppopts='-I" + Hubris::RubyHeader + "' --cpp=gcc --cppopts=-E --cppopts=-xc lib/RubyMap.chs"
  # print str
  system(str)
end

require 'spec'
require 'spec/rake/spectask'

# desc "Run the specs under spec/"
# all_examples = Spec::Rake::SpecTask.new do |t|
#   t.spec_opts = ['--options', "spec/spec.opts"]
#   t.spec_files = FileList['spec/*.rb']
# end

task :spec => ["lib/RubyMap.hs"]

task :clean do
  FileList['~/.hubris_cache/*', 'lib/*.hi', 'lib/*.ho', 'lib/RubyMap.chs.h', 'lib/RubyMap.chi','lib/RubyMap.hs', 
           'hs.out', 'stubs.c.*', 'hs.out_code*', 'rshim.c*', 'lib*.so', 'lib/*.o', 'libfoo_*.bundle', 'lib/hs.out_code.c' ].each do |f|
    begin
        File.delete(f)
    rescue
    end
  end
  system "cd sample; make clean"

end
