require File.dirname(__FILE__) + '/spec_helper.rb'

class Target
  include Hubris
end
  

describe "Target" do

  it "can be called in a block" do
    t=Target.new
    t.inline("foo (T_FIXNUM i) = T_FIXNUM (-i)")
    (1..2).each do |x|
      t.foo(x).should eql(0-x)
    end
  end
  
  # this appears not to work with GHC right now.
  # I'm not actually even that convinced that this is a valuable feature. Thoughts?
  #
  # possible sensible behaviours
  #  overwrite old behaviour silently
  #  throw an exception when overwriting is attempted
  #  print a warning
  #  silently ignore the attempt (current behaviour)
  #  ... ?
  it "can overwrite old functions" do
    t=Target.new
    t.inline("myreverse (T_STRING s) = T_STRING $ Prelude.reverse s")
    t.inline("myreverse (T_STRING s) = T_STRING $ ('a':Prelude.reverse s)")
    t.myreverse("foot").should eql("atoof")
  end

  it "can use arrays sensibly" do
    t=Target.new
    t.inline(<<EOF
mysum (T_ARRAY r) = T_FIXNUM  $ sum $ map project r 
  where project (T_FIXNUM l) = l
        project _ = 0
EOF
             )
    t.mysum([1,2,3,4]).should eql(10)
  end

  
  it "returns a haskell list as  an array" do
    t=Target.new
    t.inline(<<EOF
elts (T_FIXNUM i) = T_ARRAY $ map T_FIXNUM $ take i [1..]
elts _ = T_NIL
EOF
             )
    t.elts(5).should eql([1,2,3,4,5])
    t.elts("A Banana").should eql(nil)
  end
  
  def be_quick
    simple_matcher("a small duration") { |given| given < 1.0 }
  end
  
  it "caches its output" do
    t=Target.new
    t.inline("foobar _ = T_STRING \"rar rar rar\"")
    before = Time.now
    t.inline("foobar _ = T_STRING \"rar rar rar\"")
    after = Time.now
    (after-before).should be_quick
  end
  
end
