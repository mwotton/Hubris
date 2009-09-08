{-# LANGUAGE FlexibleInstances, ForeignFunctionInterface #-} -- , UndecidableInstances #-}
{-# INCLUDE <ruby.h> #-}

module Mapper where
-- import qualified Data.ByteString.Char8 as B -- do we need Unicode?
-- import Data.Array.CArray as CArray
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import RubyMap
import System.IO.Unsafe
newtype RString = RString CString
newtype RSymbol = RSymbol Word




class Rubyable a where
  toRuby :: a -> RValue
  fromRuby :: RValue -> a

-- instance Rubyable Int where
--   toRuby a  = T_FIXNUM a
--   fromRuby (T_FIXNUM a) = a

instance Rubyable Integer where
  toRuby a  = T_BIGNUM a
  fromRuby (T_BIGNUM a) = a

instance Num a => Rubyable a where
  toRuby a = T_BIGNUM (fromIntegral a)
  fromRuby (T_BIGNUM s) =  fromIntegral a

-- instance Rubyable Bool where
--   toRuby False  = T_FALSE
--   toRuby True   = T_TRUE
--   fromRuby T_FALSE = False
--   fromRuby T_TRUE = True

-- instance Rubyable (CArray Word RValue) where 
--   toRuby arr = T_ARRAY arr
--   fromRuby (T_ARRAY arr) = arr

-- instance Rubyable a => Rubyable [a] where
--   toRuby l = unsafePerformIO $ do
--              arr <- rb_ary_new2 (length l)
--              mapM_ (\(i,x) -> rb_ary_store i arr x) l
--              return $ T_ARRAY arr
--   fromRuby (T_ARRAY arr) = map fromRuby $ CArray.elems arr

-- instance Rubyable RString where
--     toRuby (RString cstr) = T_STRING cstr
--     fromRuby (T_STRING cstr) = RString cstr

-- instance Rubyable RSymbol where
--     toRuby (RSymbol index) = T_SYMBOL index
--     fromRuby (T_SYMBOL index) = RSymbol index

-- -- instance Integral a => Rubyable a where
--     toRuby a = T_FIXNUM (fromIntegral a)
--     fromRuby (T_FIXNUM a) = fromIntegral a

-- -- --            | T_REGEXP     
-- --               -- the array needs to be managed by ruby
-- --             | T_ARRAY (CArray Word RValue)
-- --             | T_FIXNUM Int --fixme, probably
-- --               -- the hash needs to be managed by ruby
-- --             | T_HASH  Int -- definitely FIXME - native ruby hashes, or going to translitrate?
-- -- --            | T_STRUCT     
-- --             | T_BIGNUM Integer    
-- -- --            | T_FILE       
-- --             | T_TRUE  
-- --             | T_FALSE      
-- -- --            | T_DATA       
-- --             | T_SYMBOL Word -- interned string

instance Storable RValue where
    sizeOf _ = 8 -- urgh, fixme. it's just a pointer, basically.
    alignment _ = 8

-- so here's the problem - TYPE(...) is a macro, so we can't call it from haskell.
  --   peek ptr = case toEnum $ rb_type ptr of
--                  RT_FLOAT -> T_FLOAT (rb_num2dbl ptr)
--                  RT_STRING -> T_STRING (rb_str_to_str ptr)
                 
--                  otherwise -> undefined
               
               
-- undefined -- insert definition of loadruby
 --   poke ptr a = undefined





loadRuby :: Ptr RValue -> IO RValue
loadRuby ptr = undefined

dumpRuby :: RValue -> IO (Ptr RValue)
dumpRuby rval = undefined
