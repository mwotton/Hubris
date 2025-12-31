# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)
require "hubris/version"
require 'rake'

Gem::Specification.new do |s|
  s.name        = "hubris"
  s.version     = Hubris::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ["Mark Wotton", "James Britt", "Josh Price"]
  s.email       = ["mwotton@gmail.com"]
  s.homepage    = "http://rubygems.org/gems/hubris"
  s.summary     = %q{A bridge between Ruby and Haskell}
  s.description = %q{A bridge between Ruby and Haskell}

  s.rubyforge_project = "Hubris"

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  
  s.require_paths = ["lib"]

  s.add_dependency 'rake-compiler', '>= 0.7.6'
  s.add_development_dependency 'rspec', '2.4.0'
  s.add_dependency 'rake', '~> 12.3.3'
  s.add_dependency 'bundler'
  s.add_dependency 'open4'
  s.extensions =  'Rakefile' # ext/mkrf_conf.rb'
  # system "bundle exec rake compile"
  # Rake::Task['compile'].invoke
end
