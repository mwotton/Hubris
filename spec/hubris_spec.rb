require File.dirname(__FILE__) + '/spec_helper.rb'

# Time to add your specs!
# http://rspec.info/
describe "Hubris" do
  
  it "can whine like a little baby when you pass it bad haskell" do
    lambda {Hubris::Hubris.new("broken _ = return (1 + \"a string\")")}.should raise_error(SyntaxError)
  end
  
  it "can sing like a golden bird when you treat it right, aw yeah" do
    lambda {Hubris::Hubris.new("working _ = return (toRuby $ 1 + 2)")}.should_not raise_error
  end
  
  it "can double an int in Haskell-land" do
    haskell = Hubris::Hubris.new(<<EOF
-- partial function, will probably crash and burn
double i = let j = fromRuby i in return (toRuby $ j + j)
EOF
                                 )
    haskell.double(1).should eql(2)
    haskell.double("foo").should raise_error(RuntimeError)
  end
    
end
