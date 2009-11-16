module Foo (wrap, testString, wrap2, external, can_be_called, cannot_be_called) where
can_be_called :: Int -> Int
can_be_called x = 2 * x

could_be_called :: Int -> Int
could_be_called x = 3 * x
wrap :: Int -> Int
wrap x = 3 - x

wrap2 :: Int -> Int
wrap2 x = 3 - x

cannot_be_called = map

internal = 1
external = 10

testString :: Int -> String
testString x = show x