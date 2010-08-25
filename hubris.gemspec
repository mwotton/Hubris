# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{hubris}
  s.version = "0.0.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Mark Wotton"]
  s.date = %q{2010-04-07}
  s.description = %q{Hubris is a bridge between Ruby and Haskell, between love and bondage,
between slothful indolence and raw, blazing speed. Hubris will wash
your car, lie to your boss, and salvage your love life. If you are 
very, very lucky, it might also let you get some functional goodness 
into your ruby programs through the back door.

I probably don't have to say this, but patches are very much
welcome. If you have trouble installing it, tell me, and help me
improve the docs.}
  s.email = ["mwotton@gmail.com"]
  s.extra_rdoc_files = ["Manifest.txt", "PostInstall.txt"]
  s.files = ["HISTORY.markdown", "Manifest.txt", "PostInstall.txt", "README.markdown", "Rakefile", "lib/hubris.rb", "sample/Fibonacci.hs", "sample/config.ru", "spec/spec.opts", "spec/spec_helper.rb"]
  s.post_install_message = %q{PostInstall.txt}
  s.rdoc_options = ["--main", "README.markdown"]
  s.require_paths = ["lib", "ext"]
  s.rubyforge_project = %q{hubris}
  s.rubygems_version = %q{1.3.5}
  s.summary = %q{tool to help build .so files from haskell code for use in Ruby via dl}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<rubyforge>, [">= 2.0.4"])
      s.add_development_dependency(%q<gemcutter>, [">= 0.5.0"])
      s.add_development_dependency(%q<hoe>, [">= 2.5.0"])
    else
      s.add_dependency(%q<rubyforge>, [">= 2.0.4"])
      s.add_dependency(%q<gemcutter>, [">= 0.5.0"])
      s.add_dependency(%q<hoe>, [">= 2.5.0"])
    end
  else
    s.add_dependency(%q<rubyforge>, [">= 2.0.4"])
    s.add_dependency(%q<gemcutter>, [">= 0.5.0"])
    s.add_dependency(%q<hoe>, [">= 2.5.0"])
  end
end
