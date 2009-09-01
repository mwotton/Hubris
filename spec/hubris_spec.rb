require File.dirname(__FILE__) + '/spec_helper.rb'

# just want to check it's actually possible to load a library dynamically
describe "dlload" do
  it "can actually build and load a C level dylib stupidly" do
    system "cd sample; make"
    `cd sample; ruby hsload.rb`.chomp.should eql("144")
  end
end

class Target
  include Hubris
  def foo
    14
  end
end
  

describe "Target" do
  it "can whine like a little baby when you pass it bad haskell" do
    t=Target.new
    lambda{ t.inline("broken _ = (1 + \"a string\")")}.should raise_error(SyntaxError)
  end

  it "can ignore a comment" do
    t=Target.new
    lambda {t.inline("--blah blah blah
{- another silly comment -}")}.should_not raise_error
  end
  
  it "can sing like a golden bird when you treat it right, aw yeah" do
    t=Target.new
    lambda { t.inline("working _ = T_FIXNUM (1+2)") }.should_not raise_error
  end


  it "can double an int in Haskell-land" do
    t=Target.new
    t.inline("mydouble (T_FIXNUM i) = T_FIXNUM (i + i)")

    t.mydouble(1).should eql(2)
    # and it doesn't wipe out other methods on the class
    t.foo.should eql(14)
    
    t.inline("dummy _ = T_FIXNUM 1")
    t.mydouble(1).should eql(2)
    t.dummy("dummyvar").should eql(1)
    # FIXME this one is waiting for support of Control.Exception in
    # JHC
    # Fooclever.mydouble(2.3).should raise_error(RuntimeError)
  end
  
  it "can handle doubles" do
    t = Target.new
    t.inline("triple (T_FLOAT a) = T_FLOAT (a*3.0)")
    t.triple(3.4).should eql(10.2)
  end
  
  it "can handle booleans" do
    t=Target.new
    t.inline(<<END
my_negate T_TRUE = T_FALSE
my_negate T_FALSE = T_TRUE
my_negate _ = error "argh, something went wrong"
END
             )
    t.my_negate(false).should eql(true) 
    t.my_negate(true).should eql(false) 
  end
  
  it "can handle nils too" do
    t=Target.new
    t.inline("give_me_a_nil _ = T_NIL")
    t.give_me_a_nil(1).should eql(nil)
  end
  
  it "can handle strings" do
    t=Target.new
    t.inline("reverse (T_STRING s) = T_STRING $ Prelude.reverse s")
    t.reverse("foot").should eql("toof")
  end
  
  def be_small
    simple_matcher("a small number") { |given| given == 2 or given == 4}
  end

  it "can be called in a block" do
    t=Target.new
    t.inline("foo (T_FIXNUM i) = T_FIXNUM (i*2)")
    (1..2).each do |x|
      t.foo(x).should be_small
    end
  end
end
