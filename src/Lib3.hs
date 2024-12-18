{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command(..),
    Statements(..),
    ) where


import Control.Monad (foldM)
import Control.Concurrent (Chan, readChan, writeChan, newChan)
import Control.Concurrent.STM(TVar, atomically, writeTVar, readTVarIO)
import System.IO (withFile, IOMode(..), hPutStr)
import Data.Char (isSpace)
import qualified Data.List as L

import qualified Lib2

-- Data type representing storage operations
data StorageOp = Save String (Chan ()) | Load (Chan String)

-- Loop that continuously reads storage operations from a channel and performs them
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save content notifyChan -> do
      withFile "state.txt" WriteMode $ \handle -> hPutStr handle content
      writeChan notifyChan ()
    Load responseChan -> do
      content <- filter (/= '\r') <$> readFile "state.txt"
      writeChan responseChan content
  storageOpLoop chan


data Statements = Batch [Lib2.Query] |
                  Single Lib2.Query
                  deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)


-- Parses a user command string into a Command and remaining unparsed input
parseCommand :: String -> Either String (Command, String)
parseCommand input = case L.stripPrefix "load" input of
  Just rest -> Right (LoadCommand, rest)
  Nothing -> case L.stripPrefix "save" input of
    Just rest -> Right (SaveCommand, rest)
    Nothing -> case L.stripPrefix "BEGIN" input of
      Just _ -> parseBeginBlock input
      Nothing -> case parseStatements input of
          Right (statements, rest) -> Right (StatementCommand statements, rest)
          Left err -> Left err

-- Parses a BEGIN-END block and returns a StatementCommand with parsed queries
parseBeginBlock :: String -> Either String (Command, String)
parseBeginBlock input = do
  let linesInput = map Lib2.trim (lines input)
  if not ("BEGIN" `elem` linesInput && "END" `elem` linesInput)
    then Left "Error: BEGIN-END block is incomplete"
    else do
      let blockLines = takeWhile (/= "END") $ drop 1 $ dropWhile (/= "BEGIN") linesInput
      if all null blockLines || all (all isSpace) blockLines
        then Right (StatementCommand (Batch []), "")
        else do
          queries <- mapM Lib2.parseQuery blockLines
          Right (StatementCommand (Batch queries), "")

-- Parses a single line as a query and returns a StatementCommand with the parsed query
parseStatements :: String -> Either String (Statements, String)
parseStatements input
  | L.isPrefixOf "BEGIN" input = case parseBeginBlock input of
      Right (StatementCommand s, r) -> Right (s, r)
      Right (LoadCommand, r) -> Left "Load command not allowed within BEGIN-END block"
      Right (SaveCommand, r) -> Left "Save command not allowed within BEGIN-END block"
      Left err -> Left err
  | otherwise = do
      query <- Lib2.parseQuery input
      return (Single query, "")

-- Converts a Lib2.State to a Statements representation for persistence
marshallState :: Lib2.State -> Statements
marshallState state = Batch (generateQueries (Lib2.movies state))

-- Generates Lib2.Query objects representing all movies in the state
generateQueries :: [Lib2.Movie] -> [Lib2.Query]
generateQueries movies = concatMap generateMovieCommands $ L.sortBy (\a b -> compare (Lib2.movieId a) (Lib2.movieId b)) movies

-- Generates Lib2.Query objects for a single movie (AddMovie and optionally RateMovie).
generateMovieCommands :: Lib2.Movie -> [Lib2.Query]
generateMovieCommands movie =
  let addCommand = Lib2.AddMovie (Lib2.title movie) (Lib2.genre movie) (Lib2.director movie) (Lib2.year movie)
      rateCommand = if Lib2.rating movie /= 0.0
                      then [Lib2.RateMovie (Lib2.movieId movie) (Lib2.rating movie)]
                      else []
  in [addCommand] ++ rateCommand

-- Renders Statements into a String which can be parsed back into Statements.
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) =
  "BEGIN\n" ++ concatMap (\q -> renderQuery q ++ "\n") queries ++ "END\n"

-- Renders a single Lib2.Query to a string.
renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddMovie title genre director year) =
  "add_movie(" ++ title ++ ", " ++ genre ++ ", " ++ director ++ ", " ++ show year ++ ")"
renderQuery (Lib2.RateMovie movieId rating) =
  "rate_movie(" ++ show movieId ++ ", " ++ show rating ++ ")"

-- Converts a Command to a Lib2.Query or a StorageOp.
convertCommandToLib2Query :: Command -> Either String (Maybe Lib2.Query, Maybe StorageOp)
convertCommandToLib2Query command = case command of
  StatementCommand statements -> case statements of
    Batch queries -> Right (Just (Lib2.Sequence queries), Nothing)
    Single query -> Right (Just query, Nothing)
  SaveCommand -> Right (Nothing, Just (Save "state.txt" undefined))
  LoadCommand -> Right (Nothing, Just (Load undefined))


-- Transitions the program state based on the given Command.
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, Lib2.State))
stateTransition stateVar command ioChan = do
  currentState <- readTVarIO stateVar
  case command of

    LoadCommand -> do
      responseChan <- newChan
      writeChan ioChan (Load responseChan)
      loadedContent <- readChan responseChan
      case parseBeginBlock loadedContent of
        Left parseError -> return (Left parseError)
        Right (StatementCommand (Batch queries), _) -> atomically $ do
          newState <- foldM applyQuery currentState queries
          writeTVar stateVar newState
          return (Right (Just "State loaded successfully.", newState))
        Right _ -> return (Left "Invalid BEGIN-END block structure.")

    SaveCommand -> do
      let statements = marshallState currentState
          renderedStatements = renderStatements statements
      notifyChan <- newChan
      writeChan ioChan (Save renderedStatements notifyChan)
      _ <- readChan notifyChan
      return (Right (Just "State saved successfully.", currentState))

    _ -> atomically $ do
      case convertCommandToLib2Query command of
        Left err -> return (Left err)
        Right (maybeQuery, maybeStorageOp) -> case maybeQuery of
          Nothing -> return (Right (Nothing, currentState))
          Just query -> case Lib2.stateTransition currentState query of
            Left _ -> return (Right (Nothing, currentState))
            Right (msg, newState) -> do
              writeTVar stateVar newState
              return (Right (msg, newState))
  where
    applyQuery state query = case Lib2.stateTransition state query of
      Right (_, newState) -> return newState
      Left _ -> return state