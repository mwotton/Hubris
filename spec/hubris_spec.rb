load File.dirname(__FILE__) + '/spec_helper.rb'

# # just want to check it's actually possible to load a library dynamically
# describe "dlload" do
#   it "actually builds and loads a C level dylib stupidly" do
#     system "cd sample; make"
#     `cd sample; ruby hsload.rb`.chomp.should eql("144")
#   end
# end

class Target
  include Hubris
  def foo_local
    14
  end
end

Signal.trap("INT", 'EXIT');
            
         #    ) { exit(1); raise SyntaxError, "eep, everything died" }

describe "Target" do
   it "whines like a little baby when you pass it bad haskell" do
    t = Target.new
    lambda{ t.inline("broken _ = (1 + \"a string\")")}.should raise_error(HaskellError)
  end

  it "ignores a comment" do
    t = Target.new
    lambda {t.inline("--blah blah blah
{- another silly comment -}")}.should_not raise_error
  end

  it "sings like a golden bird when you treat it right, aw yeah" do
    t = Target.new
    lambda { t.inline("working _ = T_FIXNUM (1+2)", { :no_strict => true }) }.should_not raise_error
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
    t.inline("triple (T_FLOAT a) = T_FLOAT (a*3.0)", { :no_strict => true})
    t.triple(3.4).should eql(10.2)
  end


  it "handles nils too" do
    t = Target.new
    t.inline("give_me_a_nil _ = T_NIL", { :no_strict => true})
    t.give_me_a_nil(1).should eql(nil)
  end

  it "handles strings" do
    t = Target.new
    t.inline("my_reverse (T_STRING s) = T_STRING $ Prelude.reverse s",{ :no_strict => true } )
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
    t.inline("mydouble (T_FIXNUM i) = T_FIXNUM (i + i)", { :no_strict => true } )
    t.mydouble(1).should eql(2)
    # and it doesn't wipe out other methods on the class
    t.foo_local.should eql(14)
    t.inline("dummy _ = T_FIXNUM 1", { :no_strict => true })
    t.mydouble(1).should eql(2)
    t.dummy("dummyvar").should eql(1)
    # FIXME this one is waiting for support of Control.Exception in
    # JHC
    lambda { t.mydouble(2.3)}.should raise_error(HaskellError)
    # Fooclever.mydouble(2.3).should raise_error(RuntimeError)
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
  #
  #  clearly, once I separate function binding from actual haskell compilation, this
  #  problem goes away. Overwriting becomes the sane default, and the old haskell
  #  just stops being referenced, so no linker name problems.
  
  it "can overwrite old functions" do
    pending "Haven't decided proper semantics"

    t=Target.new
    t.inline("myreverse (T_STRING s) = T_STRING $ Prelude.reverse s", {:no_strict => true })
    t.inline("myreverse (T_STRING s) = T_STRING $ ('a':Prelude.reverse s)", { :no_strict => true })
    t.myreverse("foot").should eql("atoof")
  end

  
  def be_quick
    simple_matcher("a small duration") { |given| given < 1.0 }
  end
  
  it "can insert the same code into two ruby modules" do
    pending "rejig identifiers to include module in stubs"
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
  
  
  it "doesn't affect other modules" do
    class Existing
      include Hubris
    end
    e=Existing.new
    t=Target.new
    t.inline("fun _ = T_NIL")
    lambda{ e.fun(10)}.should raise_error(NoMethodError)
  end

  it "behaves memory-wise" do
    # so, how on earth do we do this? Conceptually, we want to bind a function,
    # call it many times, and assert that memory use is (eventually) constant
    # possible approaches
    #   - caveman: ps, grep etc.
    #   - galois style (is that haskell-dtrace?)
  end
  
  it "behaves concurrently" do
    # create a bunch of ruby threads which all call a given Haskell function
    # repeatedly. Checks that we get the right result, and that we don't crash.
    t = Target.new
    no_threads = 10
    reps=1000
    t.inline("sumInts (T_FIXNUM n) = T_FIXNUM $ sum [0..n]", :no_strict => true)
    res = (0..no_threads).map { |n| (0..n).inject { |sum,n| sum+n } }
    threads = []
    lambda {
      (0..no_threads).each { |n|
        threads << Thread.start(n) { |x|
          reps.times { t.sumInts(x).should eql(res[x]) }
        }
      }
      threads.each { |t| t.join }
    }.should_not raise_error
  end
end
