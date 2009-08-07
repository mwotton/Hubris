{-# LANGUAGE ForeignFunctionInterface #-}

-- module Test where

import Foreign.C.Types
-- import Data.Map
import Maybe

-- main = putStrLn "11"

fibonacci :: Int -> Int
fibonacci n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- local_map = Data.Map.fromList [(1,2), (3,4)]



-- lookup_hs ::CInt -> CInt
-- lookup_hs = fromIntegral . Maybe.fromJust . ((flip Data.Map.lookup) local_map) . fromIntegral
-- foreign export ccall lookup_hs :: CInt -> CInt

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral

foreign export ccall fibonacci_hs :: CInt -> CInt


