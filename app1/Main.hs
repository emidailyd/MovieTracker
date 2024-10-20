{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List qualified as L
import Lib1 qualified
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )

type Repl a = HaskelineT IO a

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini =
  liftIO $ putStrLn "Welcome! Press [TAB] for auto completion."

completer :: (Monad m) => WordCompleter m
completer n =
  return $ Prelude.filter (L.isPrefixOf n) Lib1.completions

-- Evaluation: Handle each line user inputs
cmd :: String -> Repl ()
cmd input =
  case words input of
    ("add_movie":title:genre:director:year:[]) -> do
      -- Call your add movie function here, passing the parsed values
      let movieTitle = unwords title
      let movieGenre = genre
      let movieDirector = director
      let movieYear = read year :: Int
      liftIO $ putStrLn $ "Adding movie: " ++ movieTitle ++ ", Genre: " ++ movieGenre ++ ", Director: " ++ movieDirector ++ ", Year: " ++ show movieYear
    ("view_movie_details":movieId:[]) -> do
      -- Call the view_movie_details function here with movieId
      let id = read movieId :: Int
      liftIO $ putStrLn $ "Fetching details for movie ID: " ++ show id
    _ -> liftIO $ putStrLn "Unrecognized command"


main :: IO ()
main =
  evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final
