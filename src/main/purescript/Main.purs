module Main (main) where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (List(..), NonEmptyList, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Console (errorShow)
import HTTPure.Lookup ((!!))
import HTTPure.Method (Method(..))
import HTTPure.Response (badRequest, notFound, ok)
import HTTPure.Server (serve')
import Node.ChildProcess (Exit(..), SpawnOptions, StdIOBehaviour(..), defaultSpawnOptions, onExit, spawn, stderr, stdout)
import Node.FS (FileDescriptor)
import Node.Process as Process
import Options.Applicative ((<**>))
import Options.Applicative.Builder (argument, fullDesc, help, info, int, long, metavar, option, short, showDefault, str, value)
import Options.Applicative.Extra (helper)
import Options.Applicative.Types (ParseError(..), Parser, some)
import Unsafe.Coerce (unsafeCoerce)
import Utility.Options (dummyOptionalArgument, execParser')

main :: Effect Unit
main = startServer =<< (execParser' adaptOptions <<< flip info fullDesc $ settingsParser <**> helper)

type RawSettings =
  { host :: String, port :: Int, serve :: List String, run :: Maybe (List String) }
type AdaptedSettings =
  { host :: String, port :: Int, serve :: NonEmptyList String, run :: Maybe (NonEmptyList String) }

splitArgumentLists :: List String -> Tuple (List String) (Maybe (List String))
splitArgumentLists = case _ of
  "--" : list -> go Nil list
  list -> go Nil list
  where
    go prefix ("--" : suffix) = Tuple (List.reverse prefix) (Just suffix)
    go prefix Nil = Tuple (List.reverse prefix) Nothing
    go prefix (s : suffix) = go (s : prefix) suffix

settingsParser :: Parser RawSettings
settingsParser = ado
  host <- option str $
    fold [short 'b', long "bind-address", metavar "HOST", value "127.0.0.1", showDefault, help "HTTP hostname"]
  port <- option int $
    fold [short 'p', long "port", metavar "PORT", value 9512, showDefault, help "HTTP port"]
  Tuple serve run <- splitArgumentLists <<< NonEmptyList.toList <$> some (argument str $ metavar "COMMAND")

  dummyOptionalArgument "ARGUMENTS..."
  dummyOptionalArgument "-- COMMAND [ARGUMENTS...]"
  in
    { host, port, serve, run }

adaptOptions :: RawSettings -> Either ParseError AdaptedSettings
adaptOptions r@{ serve, run } = do
  serve' <- case NonEmptyList.fromList serve of
    Nothing -> Left $ ErrorMsg "Missing: COMMAND"
    Just xs -> Right xs
  run' <- case NonEmptyList.fromList <$> run of
    Just Nothing -> Left $ ErrorMsg "Missing: COMMAND to spawn"
    Just (Just xs) -> Right $ Just xs
    Nothing -> Right Nothing
  pure
    r { serve = serve', run = run' }


readZ :: String -> Either String (Array String)
readZ s = lmap printJsonDecodeError <<< decodeJson =<< jsonParser s

serveOptions :: SpawnOptions
serveOptions = defaultSpawnOptions { stdio = Just <$> [Ignore, Pipe, Pipe] }

runOptions :: SpawnOptions
runOptions = defaultSpawnOptions { stdio = Just <<< ShareFD <<< fileDescriptor <$> [0, 1, 2] }

fileDescriptor :: Int -> FileDescriptor
fileDescriptor = unsafeCoerce

startServer :: AdaptedSettings -> Effect Unit
startServer r@{ host, port, serve, run } =
  Process.onUncaughtException errorShow <* serve' listenOptions route onStart
  where
    listenOptions = { hostname: host, port, backlog: Nothing }
    command = NonEmptyList.head serve
    baseArguments = Array.fromFoldable $ NonEmptyList.tail serve

    printSettings = errorShow r { serve = Array.fromFoldable serve, run = Array.fromFoldable <$> run }
    onStart = printSettings *> case run of
      Nothing -> pure unit
      Just xs -> do
        process <- spawn (NonEmptyList.head xs) (Array.fromFoldable $ NonEmptyList.tail xs) runOptions
        onExit process case _ of
          Normally i -> Process.exit i
          BySignal _ -> Process.exit (-1)

    route { method: Get, path: [], headers } = routeParsed <<< maybe (Right []) readZ $ headers !! "Z"
    route _ = notFound

    routeParsed (Left message) = (_ { status = 404 }) <$> badRequest (message <> "\n")
    routeParsed (Right arguments) = do
      process <- liftEffect $ spawn command (baseArguments <> arguments) serveOptions
      exit <- makeAff \f -> onExit process (f <<< Right) $> nonCanceler
      case exit of
        Normally 0 -> ok $ stdout process
        _ -> badRequest $ stderr process
