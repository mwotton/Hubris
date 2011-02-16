{-# LANGUAGE ForeignFunctionInterface, CPP, TypeSynonymInstances #-}

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
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe

rubyType :: Value -> RubyType
rubyType = toEnum . rtype

-- this is awful, but I don't want to pull in C2HS - it pulls in
-- alex and happy, and they're difficult to specify as dependencies
-- in cabal.

#{enum Int,,
       T_NONE, 
       T_NIL, 
       T_OBJECT, 
       T_CLASS,
       T_ICLASS,
       T_MODULE  ,
       T_FLOAT   ,
       T_STRING  ,
       T_REGEXP  ,
       T_ARRAY   ,
       T_FIXNUM  ,
       T_HASH    ,
       T_STRUCT  ,
       T_BIGNUM  ,
       T_FILE    ,
 
       T_TRUE    ,
       T_FALSE   ,
       T_DATA    ,
       T_MATCH   ,
       T_SYMBOL  ,
 
       T_UNDEF   ,
       T_NODE    ,
       T_MASK }

data RubyType =  RT_NONE
              |  RT_NIL      
              |  RT_OBJECT   
              | RT_CLASS    
              | RT_ICLASS   
              | RT_MODULE   
              | RT_FLOAT    
              | RT_STRING   
              | RT_REGEXP   
              | RT_ARRAY    
              | RT_FIXNUM   
              | RT_HASH     
              | RT_STRUCT   
              | RT_BIGNUM   
              | RT_FILE     
                
              | RT_TRUE     
              | RT_FALSE    
              | RT_DATA     
              | RT_MATCH    
              | RT_SYMBOL   
                
              | RT_UNDEF    
              | RT_NODE     
              | RT_MASK     
                deriving (Eq, Show)

instance Enum RubyType where
 fromEnum RT_NONE     = tNone

 fromEnum RT_NIL      = tNil
 fromEnum RT_OBJECT   = tObject
 fromEnum RT_CLASS    = tClass
 fromEnum RT_ICLASS   = tIclass
 fromEnum RT_MODULE   = tModule
 fromEnum RT_FLOAT    = tFloat
 fromEnum RT_STRING   = tString
 fromEnum RT_REGEXP   = tRegexp
 fromEnum RT_ARRAY    = tArray
 fromEnum RT_FIXNUM   = tFixnum
 fromEnum RT_HASH     = tHash
 fromEnum RT_STRUCT   = tStruct
 fromEnum RT_BIGNUM   = tBignum
 fromEnum RT_FILE     = tFile

 fromEnum RT_TRUE     = tTrue
 fromEnum RT_FALSE    = tFalse
 fromEnum RT_DATA     = tData
 fromEnum RT_MATCH    = tMatch
 fromEnum RT_SYMBOL   = tSymbol
 fromEnum RT_UNDEF    = tUndef
 fromEnum RT_NODE     = tNode

 fromEnum RT_MASK     = tMask
 -- this is unnecessarily slow. fix it later.                        
 toEnum x = fromJust $ lookup x assoc
   where assoc = [( tNone   , RT_NONE)
                 ,( tNil    , RT_NIL      )
                 ,( tObject , RT_OBJECT   )
                 ,( tClass  , RT_CLASS    )
                 ,( tIclass , RT_ICLASS   )
                 ,( tModule , RT_MODULE   )
                 ,( tFloat  , RT_FLOAT    )
                 ,( tString , RT_STRING   )
                 ,( tRegexp , RT_REGEXP   )
                 ,( tArray  , RT_ARRAY    )
                 ,( tFixnum , RT_FIXNUM   )
                 ,( tHash   , RT_HASH     )
                 ,( tStruct , RT_STRUCT   )
                 ,( tBignum , RT_BIGNUM   )
                 ,( tFile   , RT_FILE     )
                  
                 ,( tTrue   , RT_TRUE     )
                 ,( tFalse  , RT_FALSE    )
                 ,( tData   , RT_DATA     )
                 ,( tMatch  , RT_MATCH    )
                 ,( tSymbol , RT_SYMBOL   )
                 ,( tUndef  , RT_UNDEF    )
                 ,( tNode   , RT_NODE     )
                  
                 ,( tMask   , RT_MASK     )]




constToRuby :: RubyConst -> Value
constToRuby = fromIntegral . fromEnum 
--  RUBY_VERSION_CODE <= 187
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
-- {# enum ruby_special_consts as RubyConst {} deriving (Eq,Show) #}

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
createException s = newCAString s >>= buildException





