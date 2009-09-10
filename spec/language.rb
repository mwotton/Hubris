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
    t.inline("myreverse (T_STRING s) = T_STRING $ Prelude.reverse s")
    t.inline("myreverse (T_STRING s) = T_STRING $ ('a':Prelude.reverse s)")
    t.myreverse("foot").should eql("atoof")
  end

  it "can use arrays sensibly" do
    t=Target.new
    t.inline(<<EOF
mysum (T_ARRAY r) = T_FIXNUM  $ sum $ map project $ map T_FIXNUM [1..3]
  where project (T_FIXNUM l) = l
        project _ = 0 -- error "expected an integer"
EOF
             )
    t.mysum([1,2,3]).should eql(6)
  end

  
#   it "returns a haskell list as  an array" do
#     t=Target.new
#     t.inline('elts _ = T_ARRAY []')
#     # (T_FIXNUM i) = T_ARRAY [] -- T_ARRAAY $ map T_FIXNUM $ take i [1..10]
#     #elts _ = T_ARRAY [T_FIXNUM 7]')
#     puts "done"
#     t.elts(5).should eql([1,2,3,4,5])
#   end




  
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
