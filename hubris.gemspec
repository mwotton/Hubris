Gem::Specification.new do |s|
  s.name = 'hubris'
  s.version = '0.0.3' # gets this from hubris.rb??

  # s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Mark Wotton", "James Britt", "Josh Price"]
  s.date = %q{2009-11-30}
  s.description = %q{A Ruby Haskell bridge}
  s.email = %q{mwotton@gmail.com}
  s.files = %w{HISTORY.markdown 
               ext/extconf.rb
               ext/HubrisStubLoader.c
               lib/hubris.rb
               Rakefile

               sample/config.ru
	       sample/Fibonacci.hs
               spec/hubris_spec.rb
               spec/spec.opts
               spec/spec_helper.rb}
  # s.extra_rdoc_files = ["README.markdown"]
#               README.markdown
  s.has_rdoc = false
  s.extensions = ["ext/extconf.rb"]
  s.homepage = 'http://github.com/mwotton/hubris'
  s.rdoc_options = ["--inline-source", "--charset=UTF-8"]
  s.require_paths = %w{lib ext}
  s.rubyforge_project = 'hubris'
  
  s.summary = 'Hubris is a Ruby Haskell bridge allowing you to call Haskell functions from your Ruby code.'
  %w{rspec open4}.each do |gem|
    s.add_dependency(gem)
  end
  puts s.extensions
end
