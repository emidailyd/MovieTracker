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

import Control.Concurrent (Chan, readChan, writeChan)
import Control.Concurrent.STM(STM, TVar, atomically, readTVar, writeTVar)
import System.IO (withFile, IOMode(..), hPutStr, hGetContents)
import qualified Data.List as L
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import qualified Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save content notifyChan -> do
      withFile "state.txt" WriteMode $ \handle -> hPutStr handle content
      writeChan notifyChan ()
    Load responseChan -> do
      content <- withFile "state.txt" ReadMode hGetContents
      writeChan responseChan content
  storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- Statements, Load, or Save.
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "load" `L.isPrefixOf` input = Right (LoadCommand, drop (length "load") input)
  | "save" `L.isPrefixOf` input = Right (SaveCommand, drop (length "save") input)
  | "BEGIN" `L.isPrefixOf` input = parseBeginBlock input
  | otherwise = do
      (statements, rest) <- parseStatements input
      return (StatementCommand statements, rest)

parseBeginBlock :: String -> Either String (Command, String)
parseBeginBlock input = do
  let linesInput = lines input
  if "BEGIN" `notElem` linesInput || "END" `notElem` linesInput
    then Left "Error: BEGIN-END block is incomplete"
    else do
      let blockLines = takeWhile (/= "END") $ drop 1 $ dropWhile (/= "BEGIN") linesInput
      if null blockLines
        then Left "Error: BEGIN-END block contains no statements"
        else do
          queries <- mapM Lib2.parseQuery blockLines
          return (StatementCommand (Batch queries), "")


-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input
  | ';' `elem` input = do
      let commands = map Lib2.trim (Lib2.splitBySemicolon input)
      queries <- mapM Lib2.parseQuery commands
      return (Batch queries, "")
  | otherwise = do
      query <- Lib2.parseQuery input
      return (Single query, "")


-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state =
  let initialState = [] 
      queries = generateQueries initialState (Lib2.movies state)
  in Batch queries

generateQueries :: [Lib2.Movie] -> [Lib2.Movie] -> [Lib2.Query]
generateQueries initialState currentState =
  let addedMovies = filter (`notElem` initialState) currentState
      removedMovies = filter (`notElem` currentState) initialState

      addMovieArgs (Lib2.Movie _ title genre director year _) = (title, genre, director, year)


      removeMovieArgs (Lib2.Movie movieId _ _ _ _ _) = movieId
  in map (uncurry4 Lib2.AddMovie . addMovieArgs) addedMovies
     ++ map (Lib2.RemoveMovie . removeMovieArgs) removedMovies

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) =
  "BEGIN\n" ++ unlines (map renderQuery queries) ++ "END\n"

-- render a single query to a string
renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddMovie title genre director year) = "add_movie " ++ title ++ " " ++ genre ++ " " ++ director ++ " " ++ show year
renderQuery (Lib2.RemoveMovie movieId) = "remove_movie " ++ show movieId
renderQuery (Lib2.ViewMovieDetails movieId) = "view_movie_details " ++ show movieId
renderQuery (Lib2.RateMovie movieId rating) = "rate_movie " ++ show movieId ++ " " ++ show rating
renderQuery (Lib2.UpdateMovie movieId title genre director year) = "update_movie " ++ show movieId ++ " " ++ maybe "" id title ++ " " ++ maybe "" id genre ++ " " ++ maybe "" id director ++ " " ++ maybe "" show year
renderQuery (Lib2.ListMovies param) = "list_movies " ++ param
renderQuery (Lib2.Sequence queries) = unwords (map renderQuery queries)


convertCommandToLib2Query :: Command -> Either String (Maybe Lib2.Query, Maybe StorageOp)
convertCommandToLib2Query (StatementCommand (Batch queries)) =
    Right (Just (Lib2.Sequence queries), Nothing)

convertCommandToLib2Query (StatementCommand (Single (Lib2.AddMovie title genre director year))) =
    Right (Just (Lib2.AddMovie title genre director year), Nothing)

convertCommandToLib2Query (StatementCommand (Single (Lib2.RemoveMovie movieId))) =
    Right (Just (Lib2.RemoveMovie movieId), Nothing)

convertCommandToLib2Query (StatementCommand (Single (Lib2.ViewMovieDetails movieId))) =
    Right (Just (Lib2.ViewMovieDetails movieId), Nothing)

convertCommandToLib2Query (StatementCommand (Single (Lib2.RateMovie movieId rating))) =
    Right (Just (Lib2.RateMovie movieId rating), Nothing)

convertCommandToLib2Query (StatementCommand (Single (Lib2.UpdateMovie movieId title genre director year))) =
    Right (Just (Lib2.UpdateMovie movieId title genre director year), Nothing)

convertCommandToLib2Query (StatementCommand (Single (Lib2.ListMovies filter))) =
    Right (Just (Lib2.ListMovies filter), Nothing)

convertCommandToLib2Query (StatementCommand (Batch queries)) =
    Right (Just (Lib2.Sequence queries), Nothing)

--TODO actually implement
convertCommandToLib2Query LoadCommand =
    Right (Nothing, Just (Load undefined))

convertCommandToLib2Query SaveCommand =
    Right (Nothing, Just (Save "" undefined))

convertCommandToLib2Query _ =
    Left "Unsupported command"


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, Lib2.State))
stateTransition stateVar command ioChan = do
    (result, maybeStorageOp) <- atomically $ do
        currentState <- readTVar stateVar
        let conversionResult = convertCommandToLib2Query command
        case conversionResult of
            Left err -> return (Left err, Nothing)
            Right (Nothing, Just storageOp) ->
                return (Right (Nothing, currentState), Just storageOp)
            Right (Just query, Nothing) -> do
                let (msg, newState) = case Lib2.stateTransition currentState query of
                        Right (msg, newState) -> (msg, newState)
                        Left _ -> (Nothing, currentState)
                writeTVar stateVar newState
                return (Right (msg, newState), Nothing)

    case maybeStorageOp of
        Just storageOp -> do
            writeChan ioChan storageOp
            return result
        Nothing -> return result
