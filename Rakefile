# TODO - want other tests/tasks run by default? Add them to the list

# remove_task :default

# task :default => [:spec, :features]

require 'rubygems'
gem 'hoe', '>= 2.1.0'
require 'hoe'
require 'fileutils'
require './lib/hubris'

Hoe.plugin :newgem
# Hoe.plugin :website
# Hoe.plugin :cucumberfeatures

# Generate all the Rake tasks
# Run 'rake -T' to see list of generated tasks (from gem root directory)
$hoe = Hoe.spec 'Hubris' do
  self.developer 'Mark Wotton, with some fixes here by James Britt', 'james@neurogami.com'
  self.post_install_message = 'PostInstall.txt' # TODO remove if post-install message not required
  self.rubyforge_name       = "hubris"
  self.summary = 'tool to help build .so files from haskel code for use in Ruby via dl' 
end

require 'newgem/tasks'
Dir['tasks/**/*.rake'].each { |t| load t }

# TODO - want other tests/tasks run by default? Add them to the list
# remove_task :default
# task :default => [:spec, :features]
