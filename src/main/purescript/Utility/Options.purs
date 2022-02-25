module Utility.Options (dummyOptionalArgument, execParser') where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process (argv)
import Options.Applicative.Builder (argument, defaultPrefs, metavar, str)
import Options.Applicative.Extra (execParserPure, handleParseResult, parserFailure)
import Options.Applicative.Types (ParseError, Parser, ParserInfo, ParserPrefs, ParserResult(..), optional)

-- | Preprocess the argument list for special handling on '--' and decorate on the parse result
execParser' :: forall a b. (a -> Either ParseError b) -> ParserInfo a -> Effect b
execParser' decorator = customExecParser' decorator defaultPrefs

customExecParser' :: forall a b. (a -> Either ParseError b) -> ParserPrefs -> ParserInfo a -> Effect b
customExecParser' decorator prefs info =
  handleParseResult <<< ((asResult <<< decorator) =<< _) =<< execParserPure prefs info <$> getArgsWithExtraDoubleDash
  where
    asResult (Left error) = Failure $ parserFailure prefs info error []
    asResult (Right b) = Success b

findDoubleDash :: Int -> Array String -> Maybe Int
findDoubleDash i a = case Array.index a i of
  Nothing -> Nothing
  Just "--" -> Just i
  Just _ -> findDoubleDash (1 + i) a

getArgsWithExtraDoubleDash :: Effect (Array String)
getArgsWithExtraDoubleDash = argv <#> \a -> case findDoubleDash 2 a of
  Nothing -> Array.drop 2 a
  Just i -> Array.slice 2 (1 + i) a <> Array.drop i a -- Include the double-dash twice

dummyOptionalArgument :: String -> Parser Unit
dummyOptionalArgument = void <<< optional <<< argument str <<< metavar
