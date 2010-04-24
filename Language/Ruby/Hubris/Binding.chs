{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

{- TODO

Rip the array trnaslation stuff out to a utility function. same with hashes.

install as package. This is a bit iffy for GHC/JHC compatibility - if we commit to
Cabal, that leaves JHC out in the cold.

perhaps need cabal file for ghc and equivalent for jhc.

also: do we want to support different versions of ruby? for the moment, you just get
whichever ruby.h is first in the search place.

-}


module Language.Ruby.Hubris.Binding where
#include "rshim.h"
#include <ruby.h>

import Control.Applicative
-- import Control.Monad
import Data.Word
-- import Foreign.Ptr
import Foreign.C.Types	
import Foreign.C.String
-- import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

-- import Foreign.Marshal.Array

{# context lib="rshim" #}

{# enum RubyType {} deriving (Eq, Show) #} -- maybe Ord?
rubyType :: Value -> RubyType
rubyType = toEnum . rtype

constToRuby :: RubyConst -> Value
constToRuby = fromIntegral . fromEnum 
--  RUBY_VERSION_CODE <= 187
#if 1
data RubyConst  = RUBY_Qfalse 
                | RUBY_Qtrue  
                | RUBY_Qnil   
                | RUBY_Qundef 

instance Enum RubyConst where
  fromEnum RUBY_Qfalse = 0
  fromEnum RUBY_Qtrue = 2
  fromEnum RUBY_Qnil = 4
  fromEnum RUBY_Qundef = 6

  toEnum 0 = RUBY_Qfalse
  toEnum 2 = RUBY_Qtrue
  toEnum 4 = RUBY_Qnil
  toEnum 6 = RUBY_Qundef
  toEnum 12 = RUBY_Qnil
#else
{# enum ruby_special_consts as RubyConst {} deriving (Eq,Show) #}
#endif

str2cstr str = rb_str2cstr str 0
type Value = CULong -- FIXME, we'd prefer to import the type VALUE directly
foreign import ccall safe "ruby.h rb_str2cstr"    rb_str2cstr    :: Value -> CInt -> IO CString
foreign import ccall safe "ruby.h rb_str_new2"    rb_str_new2    :: CString -> IO Value
foreign import ccall safe "ruby.h rb_str_new2"    rb_str_new_    :: CString -> Int -> IO Value 
foreign import ccall safe "ruby.h rb_ary_new2"    rb_ary_new2    :: CLong -> IO Value
foreign import ccall safe "ruby.h rb_ary_push"    rb_ary_push    :: Value -> Value -> IO ()
foreign import ccall safe "ruby.h rb_float_new"   rb_float_new   :: Double -> Value
foreign import ccall safe "ruby.h rb_big2str"     rb_big2str     :: Value -> Int -> IO Value
foreign import ccall safe "ruby.h rb_str_to_inum" rb_str_to_inum :: Value -> Int -> Int -> Value
-- foreign import ccall safe "ruby.h ruby_init" ruby_init :: IO ()

rb_str_new = uncurry rb_str_new_

-- we're being a bit filthy here - the interface is all macros, so we're digging in to find what it actually is
foreign import ccall safe "rshim.h rb_ary_len" rb_ary_len :: Value -> CUInt
foreign import ccall safe "rshim.h rtype"      rtype      :: Value -> Int

foreign import ccall safe "rshim.h int2fix"    int2fix    :: Int -> Value
foreign import ccall safe "rshim.h fix2int"    fix2int    :: Value -> Int
foreign import ccall safe "rshim.h num2dbl"    num2dbl    :: Value -> Double  -- technically CDoubles, but jhc promises they're the same
foreign import ccall safe "rshim.h keys"       rb_keys    :: Value -> IO Value
foreign import ccall safe "rshim.h buildException" buildException :: CString -> IO Value
-- foreign import ccall safe "ruby.h rb_funcall" rb_funcall :: Value -> ID ->

-- this line crashes jhc
foreign import ccall safe "intern.h rb_ary_entry" rb_ary_entry :: Value -> CLong -> IO Value

foreign import ccall safe "ruby.h rb_raise" rb_raise :: Value -> CString -> IO ()
foreign import ccall safe "ruby.h rb_eval_string" rb_eval_string :: CString -> IO Value

foreign import ccall safe "intern.h rb_hash_aset" rb_hash_aset :: Value -> Value -> Value -> IO ()
foreign import ccall safe "intern.h rb_hash_new" rb_hash_new :: IO Value
foreign import ccall safe "intern.h rb_hash_aref" rb_hash_aref :: Value -> Value -> IO Value


createException :: String -> IO Value
createException s = newCAString s >>= buildException --  ("puts HaskellError.methods(); HaskellError.new") >>= rb_eval_string




