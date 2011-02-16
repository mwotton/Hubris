require 'rubygems'
require 'rake'

require 'fileutils'

begin
  require 'jeweler'

  jeweler_tasks = Jeweler::Tasks.new do |gem|
    gem.name = "hubris"
    gem.summary = "a bridge between Haskell and Ruby"
    gem.description = "a bridge between Haskell and Ruby"
    gem.email = "mwotton@gmail.com"
    gem.homepage = "http://github.com/mwotton/hubris"
    gem.add_development_dependency("rspec", "= 1.3.0")
    gem.add_dependency("open4")
    gem.add_dependency("rake-compiler")
    gem.extensions = FileList["ext/**/extconf.rb"]
    gem.authors = ["Mark Wotton", "Josh Price", "James Britt"]
    gem.files.include('lib/stub.*')
  end

  $gemspec = jeweler_tasks.gemspec
  # black magic: http://karottenreibe.github.com/2009/10/25/jeweler-interlude/
  $gemspec.version = jeweler_tasks.jeweler.version
  Jeweler::GemcutterTasks.new
  
rescue LoadError
  puts "Jeweler not available. Install it with: gem install jeweler"
end

# require 'rake-compiler'
require 'rake/extensiontask'
# require 'rake/extensiontesttask'

Rake::ExtensionTask.new('stub', $gemspec) do |ext|
    # ext.cross_compile   = true
    # ext.cross_platform  = 'x86-mswin32'
    # ext.test_files      = FileList['test/c/*']
end

# CLEAN.include 'lib/**/*.so'

Rake::Task.tasks.each do |task_name|
    case task_name.to_s
    when /^native/
        task_name.prerequisites.unshift("fix_rake_compiler_gemspec_dump")
    end
end

task :fix_rake_compiler_gemspec_dump do
    %w{files extra_rdoc_files test_files}.each do |accessor|
        $gemspec.send(accessor).instance_eval {
            @exclude_procs = Array.new
        }
    end
end

desc("Build linux and windows specific gems")
task :gems do
    sh "rake clean build:native"
    sh "rake clean build:cross"
    sh "rake clean build"
end

task "build:native" => [:no_extconf, :native, :build] do
    file = "pkg/stub-#{`cat VERSION`.chomp}.gem"
    mv file, "#{file.ext}-i686-linux.gem"
end

task :no_extconf do
    $gemspec.extensions = []
end

begin
  require 'spec/rake/spectask'
  Spec::Rake::SpecTask.new(:spec) do |spec|
    spec.libs << 'lib' << 'spec'
    spec.spec_files = FileList['spec/**/*_spec.rb']
  end


  Spec::Rake::SpecTask.new(:rcov) do |spec|
    spec.libs << 'lib' << 'spec'
    spec.pattern = 'spec/**/*_spec.rb'
    spec.rcov = true
  end

  rescue LoadError

end

# task :spec => :check_dependencies

begin
  require 'reek/adapters/rake_task'
  Reek::RakeTask.new do |t|
    t.fail_on_error = true
    t.verbose = false
    t.source_files = 'lib/**/*.rb'
  end
rescue LoadError
  task :reek do
    abort "Reek is not available. In order to run reek, you must: sudo gem install reek"
  end
end

begin
  require 'roodi'
  require 'roodi_task'
  RoodiTask.new do |t|
    t.verbose = false
  end
rescue LoadError
  task :roodi do
    abort "Roodi is not available. In order to run roodi, you must: sudo gem install roodi"
  end
end

task :default => :spec

require 'rake/rdoctask'
Rake::RDocTask.new do |rdoc|
  version = File.exist?('VERSION') ? File.read('VERSION') : ""

  rdoc.rdoc_dir = 'rdoc'
  rdoc.title = "mygem #{version}"
  rdoc.rdoc_files.include('README*')
  rdoc.rdoc_files.include('lib/**/*.rb')
end


task :clean do
  FileList[File.expand_path("~/.hubris_cache/*"),
           'lib*.so', 'lib/*.o', 'libfoo_*.bundle' ].each do |f|
    begin
        File.delete(f)
    rescue
    end
  end
end
