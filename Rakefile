
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

