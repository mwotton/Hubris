{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, OverlappingInstances, UndecidableInstances#-}
module Language.Ruby.Hubris  where

--import Data.Word
import Data.Map as Map
-- import Language.Ruby.Hubris.Binding
-- import System.IO.Unsafe (unsafePerformIO)
--import Foreign.C.Types
import Language.Ruby.Hubris.Binding
--import Control.Monad (forM)
import Control.Applicative
import qualified Debug.Trace as T
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
import Control.Monad hiding (when)
import Data.Typeable

class Callable a where
  arity :: a -> Int
  
instance (Callable b, Haskellable a) => Callable (a -> b) where
  arity x = 1 + arity (undefined :: b)
  
instance Rubyable a => Callable a where
  arity x = 0
  
-- with thanks to copumpkin on #haskell and twitter

-- wrap :: (Haskellable a, Rubyable b) => (a->b) -> (Value -> Value)
-- wrap func v= unsafePerformIO $ do r <- try (evaluate $ toRuby . func $ toHaskell v)
--                                   case r of
--                                     Left (e::SomeException) -> createException (show e) `traces` "died in haskell"
--                                     Right a                 -> return a
-- -- wrapIO too? Is there a more generic way of doin
--             g this? would need a = a', b = IO c, so Rubyable b => Rubyable (IO c). (Throw away Show constraint, not necessary)
                                    
data HubrisException = HubrisException String
  deriving(Show, Typeable)


instance Exception HubrisException

-- utility stuff:
sshow :: S.ByteString -> [Char]
sshow s = Prelude.map w2c $S.unpack s
lshow :: L.ByteString -> [Char]
lshow s = Prelude.map w2c $L.unpack s

-- debugging only
trace a b = b
traces :: b -> String -> b
traces = flip trace

when :: Value -> RubyType -> a -> a
when v b c = if (rubyType v == b)
               then c
               else trace (show (rubyType v,b)) $ throw (HubrisException "failed in when")

class Haskellable a where
  toHaskell :: Value -> a

class Rubyable a where
  toRuby :: a -> Value

instance Haskellable Int where
  toHaskell v = when v RT_FIXNUM $ fix2int v

-- this is ugly, maybe we can use template haskell to remove the boilerplate?
instance (Rubyable a, Rubyable b) => Rubyable (a,b) where
  toRuby (a,b) = unsafePerformIO $ do ary <- rb_ary_new2 2
                                      rb_ary_push ary (toRuby a)
                                      rb_ary_push ary (toRuby b)
                                      return ary

instance (Rubyable a, Rubyable b, Rubyable c) => Rubyable (a,b,c) where
  toRuby (a,b,c) = unsafePerformIO $ do ary <- rb_ary_new2 3
                                        rb_ary_push ary (toRuby a)
                                        rb_ary_push ary (toRuby b)
                                        rb_ary_push ary (toRuby c)
                                        return ary

instance (Haskellable a, Haskellable b) => Haskellable (a,b) where
  toHaskell v = when v RT_ARRAY $ unsafePerformIO $
                do a <- toHaskell <$> rb_ary_entry v 0
                   b <- toHaskell <$> rb_ary_entry v 1
                   return (a,b)

instance (Haskellable a, Haskellable b, Haskellable c) => Haskellable (a,b,c) where
  toHaskell v = when v RT_ARRAY $ unsafePerformIO $
                do a <- toHaskell <$> rb_ary_entry v 0
                   b <- toHaskell <$> rb_ary_entry v 1
                   c <- toHaskell <$> rb_ary_entry v 2
                   return (a,b,c)



instance Rubyable Int where
  toRuby i = int2fix i

instance Rubyable a => Rubyable (IO a) where
  toRuby a = unsafePerformIO (a >>= return . toRuby)
instance Haskellable Integer where
  toHaskell v = case rubyType v of
                  RT_BIGNUM -> trace ("got a big") $ read  $ unsafePerformIO (rb_big2str v 10 >>= str2cstr >>= peekCString)
                  RT_FIXNUM -> trace("got a fix") $ fromIntegral $ fix2int v
                  _         -> throw (HubrisException "Integer") -- wonder if it's kosher to just let the pattern match fail...

instance  Rubyable Integer where
  toRuby i = trace ("integer to ruby") $ rb_str_to_inum (unsafePerformIO $ (newCAString $ show i) >>= rb_str_new2) 10 1

instance Haskellable Bool where
  toHaskell v = case rubyType v of
                RT_TRUE  -> True
                RT_FALSE -> False
                _        -> throw (HubrisException "Bool")

instance Rubyable Bool where
  toRuby True  = constToRuby RUBY_Qtrue
  toRuby False = constToRuby RUBY_Qfalse

instance Rubyable Double where
  toRuby d = rb_float_new d

instance Haskellable Double where
  toHaskell v = case rubyType v of
                  RT_FLOAT  -> num2dbl v
                  RT_FIXNUM -> fromIntegral $ fix2int v
                  _         -> throw (HubrisException "Double")

instance Rubyable Value where
  toRuby v = v

instance Haskellable Value where
  toHaskell v = v


instance Haskellable S.ByteString where
  toHaskell v = when v RT_STRING $ unsafePerformIO $ 
                str2cstr v >>= S.packCString  >>= \a -> return a `traces` ("strict to Haskell: " ++ sshow a)

instance Rubyable S.ByteString where
  toRuby s = unsafePerformIO $ S.useAsCStringLen s  rb_str_new
--                               \(cs,len) -> rb_str_new (cs,len) --`traces` ("sstrict back to ruby:" ++ (show $ S.unpack s))
                                                          

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
                     do -- putStrLn "Bringing hash over" 
                        keys <- rb_keys hash
                        -- putStrLn ("got the keys: "  ++ show keys)
                        l :: [Value] <- toHaskell <$> rb_keys hash

                        r <- foldM (\m k -> do -- putStrLn $ "Key is " ++ show k
                                               val <- rb_hash_aref hash k
                                               -- putStrLn $ "Val is " ++ show val
                                               return $ Map.insert (toHaskell k) 
                                                              (toHaskell val)
                                                              m)
                                    Map.empty l
                        return r
                                                   


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
