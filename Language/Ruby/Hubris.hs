{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-} 
module Language.Ruby.Hubris where

import Data.Word
import Data.Map as Map
-- import Language.Ruby.Hubris.Binding
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types
import Language.Ruby.Hubris.Binding
import Control.Monad (forM)
import Control.Applicative
import Debug.Trace
import Foreign.C.String
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import  Data.ByteString.Internal(w2c,c2w)
-- type Value = CULong
import System.IO.Unsafe
import Data.Array.IArray
import Data.Maybe
import Control.Exception
import Prelude hiding(catch)
import Monad hiding (when)
import Data.Typeable

wrap :: (Haskellable a, Rubyable b) => (a->b) -> (Value -> Value)
wrap func v= unsafePerformIO $ do r <- try (evaluate $ toRuby . func $ toHaskell v)
                                  case r of
                                    Left (e::HubrisException) -> createException "Blah" `traces` "died in haskell"
                                    Right a                   -> return a
-- wrapIO too? Is there a more generic way of doing this? would need a = a', b = IO c, so Rubyable b => Rubyable (IO c). (Throw away Show constraint, not necessary)
                                    
data HubrisException = HubrisException
  deriving(Show, Typeable)

instance Exception HubrisException

-- utility stuff:
sshow s = Prelude.map w2c $S.unpack s
lshow s = Prelude.map w2c $L.unpack s
--traces = flip trace
traces a b = a

when v b c = if (rubyType v == b)
               then c
               else throw HubrisException

class Haskellable a where
  toHaskell :: Value -> a

class Rubyable a where
  toRuby :: a -> Value

instance Haskellable Int where
  toHaskell v = when v RT_FIXNUM $ fix2int v


instance Rubyable Int where
  toRuby i = int2fix i

instance Rubyable a => Rubyable (IO a) where
  toRuby a = unsafePerformIO (a >>= return . toRuby)
instance Haskellable Integer where
  toHaskell v = case rubyType v of
                  RT_BIGNUM -> trace ("got a big") $ read  $ unsafePerformIO (rb_big2str v 10 >>= str2cstr >>= peekCString)
                  RT_FIXNUM -> trace("got a fix") $ fromIntegral $ fix2int v
                  _         -> throw HubrisException -- wonder if it's kosher to just let the pattern match fail...

instance Rubyable Integer where
  toRuby i = trace ("integer to ruby") $ rb_str_to_inum (unsafePerformIO $ (newCAString $ show i) >>= rb_str_new2) 10 1

instance Haskellable Bool where
  toHaskell v = case rubyType v of
                RT_TRUE  -> True
                RT_FALSE -> False
                _        -> throw HubrisException

instance Rubyable Bool where
  toRuby True  = constToRuby RUBY_Qtrue
  toRuby False = constToRuby RUBY_Qfalse

instance Rubyable Double where
  toRuby d = rb_float_new d

instance Haskellable Double where
  toHaskell v = case rubyType v of
                  RT_FLOAT  -> num2dbl v
                  RT_FIXNUM -> fromIntegral $ fix2int v
                  _         -> throw HubrisException

instance Rubyable Value where
  toRuby v = v

instance Haskellable Value where
  toHaskell v = v


instance Haskellable S.ByteString where
  toHaskell v = when v RT_STRING $ unsafePerformIO $ 
                do a <- str2cstr v >>= S.packCString  
                   return a `traces` ("strict to Haskell: " ++ sshow a)

instance Rubyable S.ByteString where
  toRuby s = unsafePerformIO $ S.useAsCStringLen s  $ 
                               \(cs,len) -> rb_str_new (cs,len) `traces` ("sstrict back to ruby:" ++ (show $ S.unpack s))
                                                          

instance Rubyable () where
  toRuby () = toRuby True -- ???

instance Haskellable L.ByteString where
  toHaskell v = L.fromChunks [toHaskell v]

instance Rubyable L.ByteString where
  toRuby s = let res = S.concat $ L.toChunks s
             in trace ("lazy back to ruby: " ++ show (S.unpack res)) (toRuby res)

instance Haskellable a => Haskellable [a] where
  toHaskell v = when v RT_ARRAY $ Prelude.map toHaskell $ unsafePerformIO  $ mapM (rb_ary_entry v . fromIntegral) [0..(rb_ary_len v) - 1]



instance Rubyable a => Rubyable [a] where
  toRuby l = unsafePerformIO $ do ary <- rb_ary_new2 $ fromIntegral $ Prelude.length l
                                  mapM_ (\x -> rb_ary_push ary (toRuby x))  l
                                  return ary

-- this one is probably horribly inefficient.
instance (Integral a, Ix a, Haskellable b) => Haskellable (Array a b) where
  toHaskell v = let x = toHaskell v in (listArray (0, fromIntegral $ Prelude.length x) x)

-- could be more efficient, perhaps, but it's space-efficient still thanks to laziness
instance (Rubyable b, Ix a) => Rubyable (Array a b) where
  toRuby a = toRuby $ Data.Array.IArray.elems a

instance Haskellable RubyHash where
  toHaskell v = when v RT_HASH $ RubyHash v

instance Rubyable RubyHash where
  toRuby (RubyHash v) = v


-- Nil maps to Nothing - all the other falsey values map to real haskell values.
instance Haskellable a => Haskellable (Maybe a) where 
  toHaskell v = case rubyType v of
                  RT_NIL -> Nothing                `traces` "Haskell got nothing"
                  _      -> Just (toHaskell v)     `traces` "Haskell got a value"
                                                
instance Rubyable a => Rubyable (Maybe a) where
  toRuby Nothing  = constToRuby RUBY_Qnil          `traces` "Sending ruby a nil"
  toRuby (Just a) = toRuby a                       `traces` "Sending a value back"

newtype RubyHash = RubyHash Value -- don't export constructor

instance (Ord a, Eq a, Rubyable a, Rubyable b) => Rubyable (Map.Map a b) where
  toRuby s = unsafePerformIO $ 
             do hash <- rb_hash_new
                mapM_ (\(k,v) -> rb_hash_aset hash (toRuby k) (toRuby v))  (toList s)
                return hash

instance (Ord a, Eq a, Haskellable b, Haskellable a) => Haskellable (Map.Map a b) where
  toHaskell hash = when hash RT_HASH $ unsafePerformIO $ 
                -- fromJust is legit, rb_keys will always return list
                     do l :: [Value] <- toHaskell <$> rb_keys hash
                        foldM (\m k -> do val <- rb_hash_aref hash k
                                          return $ Map.insert (toHaskell k) 
                                                              (toHaskell val)
                                                              m)
                              Map.empty l
                                                   


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

