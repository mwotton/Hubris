module Wrappers where
import Hubris
data RValue = T_FIXNUM Int
            | T_STRING String
            | T_NIL
            | T_BIGNUM Integer
            deriving (Eq, Show,Ord)

wrap :: (Haskellable a, Rubyable b) => (a->b) -> (RValue -> RValue)
wrap func ar = maybe T_NIL (toRuby . func) (toHaskell ar)

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