require File.dirname(__FILE__) + '/spec_helper.rb'

# # just want to check it's actually possible to load a library dynamically
# describe "dlload" do
#   it "actually builds and loads a C level dylib stupidly" do
#     system "cd sample; make"
#     `cd sample; ruby hsload.rb`.chomp.should eql("144")
#   end
# end

class Target
  include Hubris
  def foo
    14
  end
end

Signal.trap("INT", 'EXIT');
            
         #    ) { exit(1); raise SyntaxError, "eep, everything died" }

describe "Target" do
   it "whines like a little baby when you pass it bad haskell" do
    t = Target.new
    lambda{ t.inline("broken _ = (1 + \"a string\")")}.should raise_error(SyntaxError)
  end

  it "ignores a comment" do
    t = Target.new
    lambda {t.inline("--blah blah blah
{- another silly comment -}")}.should_not raise_error
  end

  it "sings like a golden bird when you treat it right, aw yeah" do
    t = Target.new
    lambda { t.inline("working _ = T_FIXNUM (1+2)") }.should_not raise_error
  end


  it "handles booleans" do
    t = Target.new
    t.inline(<<END
my_negate T_FALSE = T_TRUE
my_negate T_NIL = T_TRUE
my_negate _ = T_FALSE 
END
            )
    t.my_negate(false).should eql(true)
    t.my_negate(true).should eql(false)
    t.my_negate("Banana").should eql(false)
  end

  
  it "handles doubles" do
    t = Target.new
    t.inline("triple (T_FLOAT a) = T_FLOAT (a*3.0)")
    t.triple(3.4).should eql(10.2)
  end


  it "handles nils too" do
    t = Target.new
    t.inline("give_me_a_nil _ = T_NIL")
    t.give_me_a_nil(1).should eql(nil)
  end

  it "handles strings" do
    t = Target.new
    t.inline("my_reverse (T_STRING s) = T_STRING $ Prelude.reverse s")
    t.my_reverse("foot").should eql("toof")
  end

  it "handles BigInts" do
    t=Target.new
    t.inline("big_inc (T_BIGNUM i) = T_BIGNUM $ i + 1
big_inc _ = T_NIL
")
    t.big_inc(10000000000000000).should eql(10000000000000001)
  end
  
  # this one requires multiple lib linking
  it "doubles an int in Haskell-land" do
    t = Target.new
    t.inline("mydouble (T_FIXNUM i) = T_FIXNUM (i + i)")
    t.mydouble(1).should eql(2)
    # and it doesn't wipe out other methods on the class
    t.foo.should eql(14)
    t.inline("dummy _ = T_FIXNUM 1")
    t.mydouble(1).should eql(2)
    t.dummy("dummyvar").should eql(1)
    # FIXME this one is waiting for support of Control.Exception in
    # JHC
    lambda { t.mydouble(2.3)}.should raise_error(HaskellError)
    # Fooclever.mydouble(2.3).should raise_error(RuntimeError)
  end

end
