# IDEAS for new interface

# option 1 (implicit method name from haskell function)
class MyClass
  include Hubris
  def_haskell(code) 
end

# option 2 (explicit method name)
class MyClass
  def_haskell(method_name,code) 
end

# option 3 ()
class Module
  include Hubris
end

class MyClass
  inline_haskell "haskell function"
end

# option 4 (more humourous)
class Module
  include Hubris
end

class MyClass
  hubris "haskell function"
end

#-----------------------------------------
# include at the module or package level
#=========================================
# does importing as_class make sense? no, just include the module
# when interpreting Haskell module names replace . with ::

# importing std module as a ruby module/class
hubris :package => "containers", :module => "Data.Map", :as => "Data::Map"

# importing your own module (defaults to module)
hubris :package => "mypackage", :module => "MyModule" [, :as => "MyModule"] 

# implicit package handling (haskell file in my directory)
hubris :module => "haskell/shit/MyModule"

# simpler to implement option, allows more flexibility in Ruby land
module MyRubyModule
  # :packages is optional, just brings in external packages.
  hubris :module => "Data.Map", :packages => ["containers","foo"]
  # or
  hubris :source => "MyHaskellCode.hs" # , :packages => [ ... ]
  # or
  hubris :inline => "foo x = x * 2"
end


# alternative

Hubris.import :package => "containers", :module => "Data.Map"

module MyRubyModule
  include Hubris::Data::Map
end

# alternative2

module MyRubyModule
  hubris :package => "containers", :module => "Data.Map"
end


6:34:58 PM Josh Price: class Module; def hubris; self.class_eval { def self.h;"hubrified!";end };end;end
6:35:10 PM Josh Price: class B;hubris;end
