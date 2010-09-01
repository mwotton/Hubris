{-# LANGUAGE PatternGuards #-}

module Language.Ruby.Hubris.ZCode (zenc,zdec,Zname(..)) where

import Data.Char
import Data.Ix
import qualified Data.Map as M
import Numeric

zemap :: M.Map Char String
zemap = M.fromList $
    [ ('(', "ZL")
    , (')', "ZR")
    , ('[', "ZM")
    , (']', "ZN")
    , (':', "ZC")
    , ('Z', "ZZ")

    , ('z', "zz")
    , ('&', "za")
    , ('|', "zb")
    , ('^', "zc")
    , ('$', "zd")
    , ('=', "ze")
    , ('>', "zg")
    , ('#', "zh")
    , ('.', "zi")
    , ('<', "zl")
    , ('-', "zm")
    , ('!', "zn")
    , ('+', "zp")
    , ('\'', "zq")
    , ('\\', "zr")
    , ('/', "zs")
    , ('*', "zt")
    , ('_', "zu")
    , ('%', "zv")
    ]

zdmap :: M.Map String Char
zdmap = M.fromList . map (\(a, b) -> (b, a)) . M.toList $ zemap

newtype Zname = Zname String
zenc :: String -> Zname
zenc s = Zname $ concatMap (\c -> M.findWithDefault (z c) c zemap) s
    where
    z c
        | any (($ c) . inRange) [('a', 'y'), ('A', 'Z'), ('0', '9')] =
            [c]
        | otherwise =
            let
                s = showHex (ord c) "U"
                p = if inRange ('0', '9') (head s) then id else ('0' :)
            in
            'z' : p s

zdec :: String -> String
zdec "" = ""
zdec [c] = [c]
zdec (c : cs@(c' : cs'))
    | c `elem` "zZ"
    , Just x <- M.lookup [c, c'] zdmap
    = x : zdec cs'
    | c == 'z'
    , (h@(_ : _), 'U' : t) <- span isHexDigit cs
    , [(n, "")] <- readHex h
    = chr n : zdec t
    | otherwise = c : zdec cs
