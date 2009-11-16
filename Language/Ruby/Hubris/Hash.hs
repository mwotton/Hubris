module Language.Ruby.Hubris.Hash where
import qualified Language.Ruby.Hubris.Binding as Ruby
import Prelude hiding(lookup)
import Control.Applicative

newtype RubyHash = RubyHash Ruby.Value

-- can only call these functions when we have a ruby interpreter
-- initialised. shouldn't usually be a problem, but needs to be
-- done when testing from haskell.

-- to test: does this break horribly when we have multiple threads?

new = RubyHash <$> Ruby.rb_hash_new 
insert (RubyHash v) = Ruby.rb_hash_aset v

-- no Maybe here - we'd just need to test again later, as we're passing
-- a Ruby value
lookup (RubyHash v) key = Ruby.rb_hash_aref v key

-- maybe should extract strings?
keys :: RubyHash -> IO [Ruby.RValue]
keys (RubyHash v) = do Ruby.T_ARRAY res <- Ruby.fromVal <$> Ruby.rb_keys v
                       return res

toList rhash = keys rhash >>=  mapM (\k -> lookup rhash (Ruby.fromRVal k) >>= \v -> return (k,v))

