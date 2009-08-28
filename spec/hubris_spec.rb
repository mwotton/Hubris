require File.dirname(__FILE__) + '/spec_helper.rb'

# just want to check it's actually possible to load a library dynamically
describe "dlload" do
  it "can actually build and load a C level dylib stupidly" do
    system "cd sample; make"
    `cd sample; ruby hsload.rb`.chomp.should eql("144")
  end
end
describe "Hubris" do
  
  
  it "can whine like a little baby when you pass it bad haskell" do
    lambda {Hubris.build("broken _ = (1 + \"a string\")")}.should raise_error(SyntaxError)
  end

  it "can ignore a comment" do
    lambda {Hubris.build("--blah blah blah
{- another silly comment -}")}.should_not raise_error
  end
  
  it "can sing like a golden bird when you treat it right, aw yeah" do
    h = Hubris.build("working _ = T_FIXNUM (1+2)").should_not raise_error
    h.working(1).should eql 3
  end


  it "can double an int in Haskell-land" do
    haskell = Hubris.build(<<EOF
double (T_FIXNUM i) = T_FIXNUM (i + i)
EOF
                                 )
    haskell.double(1).should eql(2)
    haskell.double("foo").should raise_error(RuntimeError)
  end
  

end
