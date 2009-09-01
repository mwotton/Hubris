require File.dirname(__FILE__) + '/spec_helper.rb'

class Target
  include Hubris
end
  

describe "Target" do
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
  
  it "can overwrite old functions" do
    t=Target.new
    t.inline("reverse (T_STRING s) = T_STRING $ Prelude.reverse s")
    t.inline("reverse (T_STRING s) = T_STRING $ ('a':Prelude.reverse s)")
    t.reverse("foot").should eql("atoof")
  end

  it "can use arrays sensibly" do
    t=Target.new
    t.inline(<<EOF
mysum (RT_ARRAY r) = sum $ map project r
  where project (RT_FIXNUM l) = l
        project _ = error "expected an integer"
EOF
             )
    t.mysum([1,2,3]).should eql(6)
    
  end
  
#   def be_quick
#     simple_matcher("a small duration") { |given| given < 1.0 }
#   end
  
#   it "caches its output" do
#     t=Target.new
#     t.inline("foobar _ = T_STRING \"rar rar rar\"")
#     before = Time.now
#     t.inline("foobar _ = T_STRING \"rar rar rar\"")
#     after = Time.now
#     (after-before).should be_quick
#   end
  
end
