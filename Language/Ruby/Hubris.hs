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



wrap :: (Haskellable a, Show b, Rubyable b) => (a->b) -> (Value -> Value)
wrap func ar = case (toHaskell ar) of
                 Just a -> toRuby $ func a
                 Nothing -> unsafePerformIO $ Prelude.putStrLn "exception:" >> createException "BLAh" --  fromRVal T_NIL  -- fixme - create an exception

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

class Haskellable a where
  toHaskell :: Value -> Maybe a

class Rubyable a where
  toRuby :: a -> Value

instance Haskellable Int where
  toHaskell v = case rubyType v of
                  RT_FIXNUM -> Just $ fix2int v
                  _           -> Nothing

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
instance Haskellable ByteString where
  toHaskell v = case rubyType v of
                  RT_STRING -> Just $ unsafePerformIO $ str2cstr v >>= packCString
                  _         -> Nothing

instance Rubyable ByteString where
  toRuby s = unsafePerformIO $ useAsCString s rb_str_new2


-- instance Rubyable a => Rubyable [a] where
--   toRuby _ = error "List of as"

--instance Haskellable a => Haskellable [a] where
--  toHaskell = fromVal $ unsafePerformIO $ createException "Haskellable to lists"

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

-- instance (Rubyable a, Rubyable b) => Rubyable (Map.Map a b) where
--   toRuby s = unsafePerformIO $ 
--              do hash <- rb_hash_new
--                 forM (toList s)
--                      (\(k,v) -> rb_hash_aset hash (fromRVal $ toRuby k) (fromRVal $ toRuby v))
--                 return $ T_HASH hash
