require File.dirname(__FILE__) + '/spec_helper.rb'

class Target
  include Hubris
end

class Target2
  include Hubris
end

describe "Target" do

  it "can be called in a block" do
    t=Target.new
    t.inline("foo (T_FIXNUM i) = T_FIXNUM (-i)", { :no_strict => true })
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
  #  (actually, sometimes you get the old one, sometimes you get the new one. SPOOKY.
  #  ... ?
  it "can overwrite old functions" do
    t=Target.new
    t.inline("myreverse (T_STRING s) = T_STRING $ Prelude.reverse s", {:no_strict => true })
    t.inline("myreverse (T_STRING s) = T_STRING $ ('a':Prelude.reverse s)", { :no_strict => true })
    t.myreverse("foot").should eql("atoof")
  end

  it "can use arrays sensibly" do
    t=Target.new
    t.inline(
"mysum (T_ARRAY r) = T_FIXNUM  $ sum $ map project r 
  where project (T_FIXNUM l) = l
        project _ = 0" , {:no_strict => true })
      
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
  
#   it "handles hashes" do
#     t=Target.new
#     t.inline(<<EOF
# use_hash (T_HASH h) = case h ! (T_STRING "
# EOF
#              )
   
#   end
  
  def be_quick
    simple_matcher("a small duration") { |given| given < 1.0 }
  end
  
  it "can insert the same code into two ruby modules" do
    t=Target.new
    u=Target2.new
    t.inline("foobar _ = T_STRING \"rar rar rar\"")
    u.inline("foobar _ = T_STRING \"rar rar rar\"")
    t.foobar(nil).should eql("rar rar rar")
    u.foobar(nil).should eql("rar rar rar")
    puts t.foobar(nil)
    puts u.foobar(nil)
  end
 
  it "throws an exception on partial match" do
    t=Target.new
    t.inline("foo2 T_NIL = T_TRUE", { :no_strict => true })
    lambda{ t.foo2(1) }.should raise_error(HaskellError)
  end
  
  it "caches its output" do
    t=Target.new
    u=Target.new
    t.inline("foobar _ = T_STRING \"rar rar rar\"")
    before = Time.now
    u.inline("foobar _ = T_STRING \"rar rar rar\"")
    after = Time.now
    (after-before).should be_quick
  end

  it "catches incomplete code unless you turn no_strict on" do
    t=Target.new
    lambda { t.inline("incomplete T_NIL = T_TRUE") }.should raise_error(HaskellError)
  end
  
end
