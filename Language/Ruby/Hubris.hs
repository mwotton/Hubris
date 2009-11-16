{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-} 
module Language.Ruby.Hubris where

import Data.Word
import Data.Map as Map
-- import Language.Ruby.Hubris.Binding
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types
import Language.Ruby.Hubris.Binding
import Control.Monad (forM)
-- type Value = CULong



wrap :: (Haskellable a, Rubyable b) => (a->b) -> (Value -> Value)
wrap func ar = fromRVal $ case (toHaskell $ fromVal ar) of
                Just a ->  toRuby $ func a
                Nothing -> T_NIL

-- fromVal :: Value -> RValue
-- fromRVal ::RValue -> Value
-- fromVal = undefined
-- fromRVal = undefined

class Haskellable a where
  toHaskell :: RValue -> Maybe a

class Rubyable a where
  toRuby :: a -> RValue

instance Haskellable Int where
  toHaskell (T_FIXNUM i) = Just i
  toHaskell _ = Nothing

instance Rubyable Int where
  toRuby i = T_FIXNUM i

instance Haskellable Integer where
  toHaskell (T_BIGNUM i) = Just i
  toHaskell _ = Nothing

instance Rubyable Integer where
  toRuby i = T_BIGNUM i

instance Haskellable Bool where
  toHaskell T_TRUE  = Just True
  toHaskell T_FALSE = Just False
  toHaskell _       = Nothing

instance Rubyable Bool where
  toRuby True = T_TRUE
  toRuby False = T_FALSE

instance Rubyable Double where
  toRuby d = T_FLOAT d

instance Haskellable Double where
  toHaskell (T_FLOAT f)  = Just f
  toHaskell (T_FIXNUM i) = Just $ fromIntegral i -- does this make sense?
  toHaskell _ = Nothing 

instance Haskellable String where
  toHaskell (T_STRING s) = Just s
  toHaskell _ = Nothing
instance Rubyable String where
  toRuby s = (T_STRING s)


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

instance (Rubyable a, Rubyable b) => Rubyable (Map.Map a b) where
  toRuby s = unsafePerformIO $ 
             do hash <- rb_hash_new
                forM (toList s)
                     (\(k,v) -> rb_hash_aset hash (fromRVal $ toRuby k) (fromRVal $ toRuby v))
                return $ T_HASH hash
