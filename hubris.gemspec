Gem::Specification.new do |s|
  s.name = %q{hubris}
  s.version = "0.0.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Mark Wotton", "James Britt", "Josh Price"]
  s.date = %q{2009-08-16}
  s.description = %q{A Ruby Haskell bridge}
  s.email = %q{mwotton@gmail.com}
  s.extra_rdoc_files = ["README.markdown"]
  s.files = ["bin/jhc_builder", "History.txt", "lib/extconf.rb", "lib/hubris.rb", "lib/Mapper.hs", "lib/rshim.c", "lib/rshim.h", "lib/RubyMap.chs", "Rakefile", "README.markdown", "sample/hsload.rb", "sample/Makefile", "sample/Test.hs", "spec/hubris_spec.rb", "spec/spec.opts", "spec/spec_helper.rb", "tasks/rspec.rake"]
  s.has_rdoc = false
  s.homepage = %q{http://github.com/mwotton/hubris}
  s.rdoc_options = ["--inline-source", "--charset=UTF-8"]
  s.require_paths = ["lib"]
  #  s.rubyforge_project = %q{hubris}
  s.rubygems_version = %q{1.3.0}
  s.summary = %q{Hubris is a Ruby Haskell bridge allowing you to call Haskell functions from your Ruby code.}
  %w{rspec open4}.each do |gem|
    s.add_dependency(gem)
  end
end
