module Hint.Parsers

where

import Prelude hiding(span)

import Hint.Base
import qualified Hint.Compat as Compat

import Control.Monad.Trans ( liftIO )

import qualified Hint.GHC as GHC

data ParseResult = ParseOk | ParseError GHC.SrcSpan GHC.Message

parseExpr :: MonadInterpreter m => String -> m ParseResult
parseExpr = runParser GHC.parseStmt

parseType :: MonadInterpreter m => String -> m ParseResult
parseType = runParser GHC.parseType

runParser :: MonadInterpreter m => GHC.P a -> String -> m ParseResult
runParser parser expr =
    do dyn_fl <- runGhc GHC.getSessionDynFlags
       --
       buf <- liftIO $ GHC.stringToStringBuffer expr
       --
       let parse_res = GHC.unP parser (Compat.mkPState dyn_fl buf GHC.noSrcLoc)
       --
       case parse_res of
           GHC.POk{}            -> return ParseOk
           --
           GHC.PFailed span err -> return (ParseError span err)

failOnParseError :: MonadInterpreter m
                 => (String -> m ParseResult)
                 -> String
                 -> m ()
failOnParseError parser expr = mayFail go
    where go = do parsed <- parser expr
                  --
                  -- If there was a parsing error,
                  -- do the "standard" error reporting
                  case parsed of
                      ParseOk             -> return (Just ())
                      --
                      ParseError span err ->
                          do -- parsing failed, so we report it just as all
                             -- other errors get reported....
                             logger <- fromSession ghcErrLogger
                             liftIO $ logger GHC.SevError
                                             span
                                             GHC.defaultErrStyle
                                             err
                             --
                             -- behave like the rest of the GHC API functions
                             -- do on error...
                             return Nothing
