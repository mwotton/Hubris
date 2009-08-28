
file 'sample/libdynhs.so' => ['sample/Test.hs'] do
  sh "./bin/jhc_builder.sh sample/Test.hs"
end

#file 'lib/RubyMap.hs' => ['lib/RubyMap.chs'] do
#  sh "cd lib; c2hs -v --cppopts='-I/opt/local/include/ruby-1.9.1/ruby' --cpp=gcc --cppopts=-E --cppopts=-xc RubyMap.chs; cd .."
#end
