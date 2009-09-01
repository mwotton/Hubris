{-# LANGUAGE ForeignFunctionInterface #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
module RubyMap where
#include "rshim.h"
#include <ruby.h>

-- import Data.Array.CArray as CArray
import Data.Word
import Foreign.Ptr
import Foreign.C.Types	
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
-- import PackedString
{# context lib="rshim" #}
{# enum RubyType {} deriving (Eq, Show) #} -- maybe Ord?
{# enum ruby_special_consts as RubyConsts {} deriving (Eq,Show) #}

type Value = CULong -- FIXME, we'd prefer to import the type VALUE directly
foreign import ccall unsafe "ruby.h rb_str2cstr" rb_str2cstr :: Value -> CInt -> CString
foreign import ccall unsafe "ruby.h rb_str_new2" rb_str_new2 :: CString -> Value
foreign import ccall unsafe "ruby.h rb_ary_new2" rb_ary_new :: Int -> IO Value
foreign import ccall unsafe "ruby.h rb_ary_store" rb_ary_store :: Value -> Int -> Value -> IO ()
foreign import ccall unsafe "ruby.h rb_float_new" rb_float_new :: Double -> Value

-- we're being a bit filthy here - the interface is all macros, so we're digging in to find what it actually is
foreign import ccall unsafe "rshim.h rtype" rtype :: Value -> Int
foreign import ccall unsafe "rshim.h int2fix" int2fix :: Int -> Value
foreign import ccall unsafe "rshim.h fix2int" fix2int ::  Value -> Int

foreign import ccall unsafe "rshim.h num2dbl" num2dbl :: Value -> Double  -- technically CDoubles, but jhc promises they're the same

foreign import ccall unsafe "stdio.h puts" puts :: CString -> IO ()


-- all values in here need to be allocated and tracked by ruby.
-- ByteStrings... hm. Probably better to keep them as C-side ruby strings.
-- better come back and expand this later
data RValue = T_NIL  
--            | T_OBJECT 
--             | T_CLASS      
--             | T_MODULE     
            | T_FLOAT Double
            | T_STRING String
--            | T_REGEXP     
              -- the array needs to be managed by ruby
--            | T_ARRAY (CArray Word RValue)
            | T_FIXNUM Int --fixme, probably
              -- the hash needs to be managed by ruby
            | T_HASH  Int -- definitely FIXME - native ruby hashes, or going to translitrate?
--            | T_STRUCT     
            | T_BIGNUM Integer    
--            | T_FILE
     -- technically, these are mapping over the types True and False,
     -- I'm going to treat them as values, though.
            | T_TRUE  
            | T_FALSE      
--            | T_DATA       
            | T_SYMBOL Word -- interned string



-- qnil = 4
-- qfalse = 0
-- qtrue = 2

toRuby :: RValue -> Value
toRuby r = case r of
           T_FLOAT d -> rb_float_new d
           -- need to take the address of the cstr, just cast it to a value
           -- sadly no bytestrings yet - unpack it to a list. yeah it's ugly.
           T_STRING str -> rb_str_new2 $ unsafePerformIO $ newCAString str
           T_FIXNUM i -> int2fix i
           -- so this is just bizarre - there's no boolean type. True and False have their own types
           -- as well as their own values.
           T_TRUE  ->  fromIntegral $ fromEnum RUBY_Qtrue
           T_FALSE ->  fromIntegral $ fromEnum RUBY_Qfalse
           T_NIL   ->  fromIntegral $ fromEnum RUBY_Qnil
           T_BIGNUM _ -> error "No implementation for Bignums yet"
           x -> error ("sorry, haven't implemented that yet.")

fromRuby :: Value -> RValue
fromRuby v = case target of
               RT_NIL -> T_NIL
               RT_FIXNUM -> T_FIXNUM $ fix2int v
               RT_STRING -> T_STRING $ unsafePerformIO $ peekCString $ rb_str2cstr v 0
               RT_FLOAT ->  T_FLOAT $ num2dbl v
               RT_BIGNUM -> error "no bignum yet"
               RT_TRUE -> T_TRUE
               RT_FALSE -> T_FALSE
               RT_NIL   -> T_NIL
               _ -> error (show target)
  where target :: RubyType
        target = toEnum $ rtype v