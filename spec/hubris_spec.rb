load File.dirname(__FILE__) + '/spec_helper.rb'
require "hubris"
Hubris.add_packages %w(base bytestring)

# # just want to check it's actually possible to load a library dynamically
# describe "dlload" do
#   it "actually builds and loads a C level dylib stupidly" do
#     system "cd sample; make"
#     `cd sample; ruby hsload.rb`.chomp.should eql("144")
#   end
# end

class Target
  def foo_local
    14
  end
end

Signal.trap("INT", 'EXIT');

describe "Target" do  
  it "whines like a little baby when you pass it bad haskell" do
    lambda{ class Foo; hubris :inline => "broken _ = (1 + \"a string\")", :no_strict => true; end}.should raise_error(HaskellError)
  end

  it "ignores a comment" do
    lambda {
      class Foo; hubris :inline => "--blah blah blah
{- another silly comment -}
foo :: Bool -> Bool
foo True = False
foo False = True"; end
    }.should_not raise_error
  end

  it "sings like a golden bird when you treat it right, aw yeah" do
    #    t = Target.new
    #    lambda { t.inline("working _ = T_FIXNUM (1+2)", { :no_strict => true }) }.should_not raise_error
    lambda { class Foo; hubris :inline => "working :: Integer -> Integer; working a = 2*a"; end}.should_not raise_error
  end


  #   it "handles booleans" do
  #     class Bar
  #       hubris :inline => "my_negate True = False;my_negate False = True", :no_strict => true
  #     end
  #     t = Bar.new
  #     # puts t.methods
  #     t.my_negate(false).should eql(true)
  #     t.my_negate(true).should eql(false)
  #     lambda{ t.my_negate("Banana")}.should raise_error
  #   end
  
  it "handles doubles" do
    class Doubler
      hubris :inline => "triple :: Double -> Double; triple a = a*3.0", :no_strict => true
    end
    d = Doubler.new

    puts "written"
    d.triple(3.4).should eql(10.2)
    puts "got doubled"
  end
end

describe "Strings" do
  it "can reverse a string" do 
    class Stringer
      hubris :inline => "import Data.ByteString; my_reverse::ByteString->ByteString; my_reverse s = Data.ByteString.reverse s", :no_strict => true
    end
    Stringer.new.my_reverse("foot").should eql("toof")
  end
end

describe "BigInt" do
  it "handles BigInts" do
    class Bigint
      hubris :inline => "big_inc :: Integer -> Integer; big_inc i = i + 1"
    end
    b = Bigint.new
    b.big_inc(10000000000000000).should eql(10000000000000001)
    b.big_inc(1).should eql(2)
  end
end

describe 'Multiple' do
  # this one requires multiple lib linking
  it "can load multiple libs" do
    class Multiple
      def foo_local
        14
      end
      hubris :inline => "mydouble::Int->Int; mydouble i =(i + i)" 
      hubris :inline => "incr::Int->Int;incr i = 1+i"
    end
    t=Multiple.new
    t.mydouble(1).should eql(2)
    # and it doesn't wipe out other methods on the class
    t.foo_local.should eql(14)
    t.incr(3).should eql(4)
    # FIXME this one is waiting for support of Control.Exception in
    # JHC
    # lambda { t.mydouble(2.3)}.should raise_error(HaskellError)
    # Fooclever.mydouble(2.3).should raise_error(RuntimeError)
  end
end

describe 'Arrays' do
  it "can use arrays sensibly" do
    class ArrayTest
      hubris :inline => "mysum :: [Int] -> Int; mysum [] = 0; mysum (x:xs) = x + sum xs"
    end
    
    ArrayTest.new.mysum([1,2,3,4]).should eql(10)
    
    it "returns a haskell list as  an array" do
      class ArrayTest2
        hubris :inline => "elts :: Int -> [Int]; elts i = take i [1..]"
      end
      t=ArrayTest2.new
      t.elts(5).should eql([1,2,3,4,5])
      t.elts("A Banana").should eql(nil)
    end
  end

  #   it "handles hashes" do
  #     t=Target.new
  #     t.inline(<<EOF
  # use_hash (T_HASH h) = case h ! (T_STRING "
  # EOF
  #              )
  
  #   end


end
describe "Target" do
  
  it "can be called in a block" do
    class T2
      hubris :inline => "foo::Integer->Integer;foo i = -i"
    end
    t = T2.new
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
    class Overlapping
      hubris :inline => "myid::Int -> Int; myid i = i"
      hubris :inline => "myid::Int -> Int; myid i = i+1"
    end
    t=Overlapping.new
    t.myid(1).should eql(2)
  end

  
  def be_quick
    simple_matcher("a small duration") { |given| given < 1.0 }
  end
end

describe "Exceptions" do
  it "throws an exception on partial match" do
    class BoolFunc
      hubris :inline => "mynot :: Bool -> Bool; mynot True = False; mynot False = True"
    end
    t=BoolFunc.new
    lambda{ t.mynot(true) }.should_not raise_error(HaskellError)
    lambda{ puts t.mynot("blah") }.should raise_error(HaskellError)
  end

  it "catches incomplete code unless you turn no_strict on" do
    t=Target.new
    lambda {
      class Incomplete
        hubris :inline => "incomplete :: Int -> Bool; incomplete 1 = True"
      end
    }.should raise_error(HaskellError)
    lambda { 
      class IncompleteButOk
        hubris :inline => "incomplete :: Int -> Bool; incomplete 1 = True" , :no_strict => true
      end
    }.should_not raise_error()
    
  end
end

describe 'Idempotence' do 
  it "doesn't affect other modules" do
    class Existing
    end

    class Target
      hubris :inline => "fun :: Int -> Int; fun x = x+1"
    end
    e=Existing.new
    t=Target.new
    lambda{ e.fun(10)}.should raise_error(NoMethodError)
    lambda{ t.fun(10)}.should_not raise_error(NoMethodError)
    t.fun(10).should eql(11)
  end
  
  it "can insert the same code into two ruby modules" do
    class Foo10
      hubris :inline => "foobar::Double -> Double;foobar n = n+1.0"
    end
    class Foo11
      hubris :inline => "foobar::Double -> Double;foobar n = n+1.0"
    end

    Foo10.new.foobar(1.0).should eql(2.0)
    Foo11.new.foobar(1.0).should eql(2.0)

  end
end

describe 'Realworld' do
  it "can handle the bytestring lib" do
    system("rm /var/hubris/cache/Data.ByteString.bundle;
Hubrify Data.ByteString;
")
    # FIXME zencode module names properly
    class ByteString
      hubris :module => "Data.ByteString"
    end
    
    b = ByteString.new
    b.sort("zabcdfg").should == "abcdfgz"
  end
  
end

describe 'Performance' do
  it "caches its output" do
    pending "not really relevant any more"
    t=Target.new
    u=Target.new
    t.inline("foobar _ = T_STRING \"rar rar rar\"")
    before = Time.now
    u.inline("foobar _ = T_STRING \"rar rar rar\"")
    after = Time.now
    (after-before).should be_quick
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
    
    no_threads = 10
    reps=1000
    class ConcTest
      hubris :inline => "sumInts :: Int -> Int; sumInts n = sum [0..n]"
    end
    t = ConcTest.new
    
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
