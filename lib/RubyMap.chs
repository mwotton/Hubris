{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module RubyMap where
#include "rshim.h"
#include <ruby.h>

import Data.Array.CArray as CArray
import Data.Word
import Foreign.Ptr
import Foreign.C.Types	
import Foreign.C.String

{# context lib="rshim" #}
{# enum RubyType {} deriving (Eq) #} -- maybe Ord?

-- can we have a type for Value, plz?
type Value = Ptr () -- fixme

-- we're being a bit filthy here - the interface is all macros, so we're digging in to find what it actually is
foreign import ccall unsafe "rshim.h rtype" rb_type :: Value -> CInt
foreign import ccall unsafe "ruby.h rb_num2dbl" rb_num2dbl :: Value -> CDouble
foreign import ccall unsafe "ruby.h rb_str_to_str" rb_str_to_str :: Value -> CString
foreign import ccall unsafe "ruby.h rb_ary_new2" rb_ary_new :: Int -> IO Value
foreign import ccall unsafe "ruby.h rb_ary_store" rb_ary_store :: Value -> Int -> Value -> IO ()


-- all values in here need to be allocated and tracked by ruby.
-- ByteStrings... hm. Probably better to keep them as C-side ruby strings.
-- better come back and expand this later
data RValue = T_NIL  
--            | T_OBJECT 
--             | T_CLASS      
--             | T_MODULE     
            | T_FLOAT Double
            | T_STRING CString
--            | T_REGEXP     
              -- the array needs to be managed by ruby
            | T_ARRAY (CArray Word RValue)
            | T_FIXNUM Int --fixme, probably
              -- the hash needs to be managed by ruby
            | T_HASH  Int -- definitely FIXME - native ruby hashes, or going to translitrate?
--            | T_STRUCT     
            | T_BIGNUM Integer    
--            | T_FILE       
            | T_TRUE  
            | T_FALSE      
--            | T_DATA       
            | T_SYMBOL Word -- interned string
