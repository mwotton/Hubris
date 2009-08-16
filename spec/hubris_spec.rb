require File.dirname(__FILE__) + '/spec_helper.rb'

# Time to add your specs!
# http://rspec.info/
describe "Hubris" do
  
  it "can build the sample app" do
    # violated "Be sure to write your specs"
  end
  
  it "can whine like a little baby when you pass it bad haskell" do
    lambda {Hubris::Hubris.new("broken _ = return (T_FIXNUM (1 + \"a string\")")}.should raise_error(SyntaxError)
  end
  
  it "can sing like a golden bird when you treat it right, aw yeah" do
    lambda {Hubris::Hubris.new("working _ = return (T_FIXNUM (1 + 2))")}.should_not raise_error
  end
  
  it "can double an int in Haskell-land" do
    haskell = Hubris::Hubris.new(<<EOF
-- partial function, will probably crash and burn
double (T_FLOAT d)  = return $ T_FLOAT (x+x)
double (T_FIXNUM i) = return $ T_FIXNUM (x+x)
double (T_BIGNUM i) = return $ T_BIGNUM (x+x)
EOF
                                 )
    haskell.double(1).should eql(2)
    haskell.double("foo").should raise_error(RuntimeError)
  end
    
end
