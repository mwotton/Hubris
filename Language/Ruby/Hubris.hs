{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-} 
module Language.Ruby.Hubris where

import Data.Word
import Data.Map as Map
-- import Language.Ruby.Hubris.Binding
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types
import Language.Ruby.Hubris.Binding
import Control.Monad (forM)
import Debug.Trace
import Foreign.C.String
import Data.ByteString
-- type Value = CULong
import System.IO.Unsafe
import Data.Array.IArray
import Monad hiding (when)

wrap :: (Haskellable a, Show b, Rubyable b) => (a->b) -> (Value -> Value)
wrap func ar = case (toHaskell ar) of
                 Just a -> toRuby $ func a
                 Nothing -> unsafePerformIO $ createException "BLAh" 

-- wrapshow :: (Haskellable a, Show b, Show a, Rubyable b) => (a->b) -> (Value -> Value)
-- wrapshow func ar = trace "Wrap called" $ let rv = fromVal ar
--                                          in trace (unlines ["raw:" ++ show ar,
--                                                             "in:" ++ show rv]) $ fromRVal $ 
--                                             case (toHaskell rv) of
--                                             Just a ->  let v = func a in
--                                                            trace("out" ++ show v) (toRuby v)
--                                             Nothing -> T_NIL


-- fromVal :: Value -> RValue
-- fromRVal ::RValue -> Value
-- fromVal = undefined
-- fromRVal = undefined

when v b c = guard (rubyType v == b) >> return c

class Haskellable a where
  toHaskell :: Value -> Maybe a

class Rubyable a where
  toRuby :: a -> Value

instance Haskellable Int where
  toHaskell v = when v RT_FIXNUM $ fix2int v


instance Rubyable Int where
  toRuby i = int2fix i

instance Haskellable Integer where
  toHaskell v = case rubyType v of
                  RT_BIGNUM -> Just $ read  $ unsafePerformIO (rb_big2str v 10 >>= str2cstr >>= peekCString)
                  RT_FIXNUM -> Just $ fromIntegral $ fix2int v
                  _         -> Nothing

instance Rubyable Integer where
  toRuby i = rb_str_to_inum (unsafePerformIO $ (newCAString $ show i) >>= rb_str_new2) 10 1

instance Haskellable Bool where
  toHaskell v = case rubyType v of
                RT_TRUE  -> Just True
                RT_FALSE -> Just False
                _        -> Nothing

instance Rubyable Bool where
  toRuby True  = constToRuby RUBY_Qtrue
  toRuby False = constToRuby RUBY_Qfalse

instance Rubyable Double where
  toRuby d = rb_float_new d

instance Haskellable Double where
  toHaskell v = case rubyType v of
                  RT_FLOAT  -> Just $ num2dbl v
                  RT_FIXNUM -> Just $ fromIntegral $ fix2int v
                  _         -> Nothing

instance Rubyable Value where
  toRuby v = v

instance Haskellable Value where
  toHaskell v = Just v

instance Haskellable ByteString where
  toHaskell v = when v RT_STRING $ unsafePerformIO $ str2cstr v >>= packCString

instance Rubyable ByteString where
  toRuby s = unsafePerformIO $ useAsCString s rb_str_new2

instance Haskellable [Value] where
  toHaskell v = when v RT_ARRAY $ unsafePerformIO  $ mapM (rb_ary_entry v . fromIntegral) [0..(rb_ary_len v) - 1]



instance Rubyable a => Rubyable [a] where
  toRuby l = unsafePerformIO $ do ary <- rb_ary_new2 $ fromIntegral $ Prelude.length l
                                  mapM_ (\x -> rb_ary_push ary (toRuby x))  l
                                  return ary

-- this one is probably horribly inefficient.
instance (Integral a, Ix a) => Haskellable (Array a Value) where
  toHaskell v = toHaskell v >>= \x -> return (listArray (0, fromIntegral $ Prelude.length x) x)

-- could be more efficient, perhaps, but it's space-efficient still thanks to laziness
instance (Rubyable b, Ix a) => Rubyable (Array a b) where
  toRuby a = toRuby $ Data.Array.IArray.elems a

instance Haskellable RubyHash where
  toHaskell v = when v RT_HASH $ RubyHash v

instance Rubyable RubyHash where
  toRuby (RubyHash v) = v

newtype RubyHash = RubyHash Value -- don't export constructor

instance (Ord a, Eq a, Rubyable a, Rubyable b) => Rubyable (Map.Map a b) where
  toRuby s = unsafePerformIO $ 
             do hash <- rb_hash_new
                mapM_ (\(k,v) -> rb_hash_aset hash (toRuby k) (toRuby v))  (toList s)
                return hash


-- This is a tricky case.
-- The ruby FFI wants us to pass a C callback which it can apply to each key-value pair
-- of the hash, so Haskell cannot be fully in control of the process - this makes building
-- up a Data.Map object in the natural way a bit tricky.

-- current thoughts:
--  1. write a direct binding to the ruby API, include a C level function for getting the keys.
--     just eat the cost of transferring through a keys call + looping over the elements.
--     One big benefit - while iteration is expensive, using it as a hash table should be cheap
--     (although probably needs to stay in the IO monad, which is less convenient.)
--
--  2. write a binding to the Judy library that creates a Judy object directly. If we can convince
--     HsJudy to accept that, then we're golden - we still have to copy over, but keys operations
--     should be cheap (and hopefully lazy, but test to make sure).
--
-- These are of course not mutually exclusive.
--
-- The first should probably be a part of the base package. The second needs access to internals,
-- but should probably be an optional package. This means that in Hubris.Internals, we should expose

-- > rb_foreach :: Value {- HASH -} -> (CFunction ((Key,Value,a) -> a)) -> a -> IO a 
-- 
-- 

-- instance Haskellable (Map.Map a b ) where 
--   toHaskell (T_HASH s) = unsafePerformIO $ 
--                          get_each
                         
--   toHaskell _ = Nothing

