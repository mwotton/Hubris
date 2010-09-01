{-# LANGUAGE TemplateHaskell #-}

module Interpolator where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse

-- The first string in each pair is literal, the second is a variable to
-- be interpolated.
parse :: String -> [(String, String)]
-- parse "Foo#{foo}rah#{foo}"   = [("Foo", "foo ++ bar"), ("rah", "foo")]
parse str = 
gen :: [(String, String)] -> Q Exp -> Q Exp
gen [] x = x 
-- gen ((string,variable) : xs) x = gen xs [| $x ++ $(lift string) ++ $(return $ VarE $ mkName variable) |]
gen ((string,expr) : xs) x = gen xs [| $x ++ $(lift string) ++ $(return $ lift $ parseExp expr) |]
-- gen ((string,variable) : xs) x = gen xs [| $x ++ $(lift string) ++ $(stringE variable) |]

-- Here we generate the Haskell code for the splice
-- from an input format string.
interpolate :: String -> Q Exp
interpolate s = gen (parse s) [| "" |] 
