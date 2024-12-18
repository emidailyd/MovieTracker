{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    parseString,
    State(..),
    emptyState,
    stateTransition,
    Movie(..),
    trim,
    splitBySemicolon,
    movies
    ) where

import qualified Data.List as L
import qualified Data.Char as C

-- | An entity which represents a movie.
-- This includes the movie ID, title, genre, director, year, and rating.
data Movie = Movie
  { movieId :: Int
  , title :: String
  , genre :: String
  , director :: String
  , year :: Int
  , rating :: Double
  } deriving(Eq, Show)

availableGenres :: [String]
availableGenres = ["Action", "Comedy", "Drama", "Horror", "SciFi", "Romance"]

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query
  = AddMovie String String String Int         -- add_movie(title, genre, director, year)
  | RemoveMovie Int                           -- remove_movie(movie_id)
  | ViewMovieDetails Int                      -- view_movie_details(movie_id)
  | RateMovie Int Double                      -- rate_movie(movie_id, rating)
  | UpdateMovie Int (Maybe String) (Maybe String) (Maybe String) (Maybe Int)  -- update_movie(movie_id, title, genre, director, year)
  | ListMovies String                         -- list_movies(all|genre:|year:)
  | Sequence [Query]                          -- multiple query commands
  deriving (Eq, Show)
  
-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input
  | ';' `elem` input =
      let commands = map trim (splitBySemicolon input)
      in case mapM parseQuery commands of
           Right queries -> Right (Sequence queries)
           Left err -> Left err
  | L.isPrefixOf "add_movie" input = parseAddMovie (drop (length "add_movie") input)
  | L.isPrefixOf "view_movie_details" input = parseViewMovieDetails (drop (length "view_movie_details") input)
  | L.isPrefixOf "rate_movie" input = parseRateMovie (drop (length "rate_movie") input)
  | L.isPrefixOf "remove_movie" input = parseRemoveMovie (drop (length "remove_movie") input)
  | L.isPrefixOf "update_movie" input = parseUpdateMovie (drop (length "update_movie") input)
  | L.isPrefixOf "list_movies" input = parseListMovies (drop (length "list_movies") input)
  | otherwise = Left "Unknown command or invalid input"


-- HELPER FUNCTIONS

-- split by semicolon
splitBySemicolon :: String -> [String]
splitBySemicolon [] = []
splitBySemicolon input =
    case break (== ';') input of
        (part, [])   -> [part]
        (part, rest) -> part : splitBySemicolon (tail rest)

-- trim spaces from a string
trim :: String -> String
trim = L.dropWhileEnd C.isSpace . L.dropWhile C.isSpace

-- split a string by commas
splitByComma :: String -> [String]
splitByComma [] = []
splitByComma input =
    case break (== ',') input of
        (part, []   ) -> [part]
        (part, rest) -> part : splitByComma (tail rest)

-- returns value inside maybe or default if its nothing
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- checks if the genre is available
validateGenre :: String -> Either String String
validateGenre genre =
  if genre `elem` availableGenres
    then Right genre
    else Left $ "Invalid genre: " ++ genre ++ ". Available genres are: " ++ unwords availableGenres

-- BNF PARSERS

-- <letter> ::= "a" | "b" | "c" | ... | "z" | "A" | "B" | ... | "Z"
parseLetter :: String -> Maybe (Char, String)
parseLetter [] = Nothing  -- no input
parseLetter (x:xs)
  | (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') = Just (x, xs)  -- valid letter found
  | otherwise = Nothing  -- not a valid letter

-- <number> ::= "0" | "1" | "2" | ... | "9"
parseNumber :: String -> Maybe (Int, String)
parseNumber [] = Nothing  -- No input to parse
parseNumber input = 
  let (digits, rest) = span C.isDigit input
  in if null digits
       then Nothing  -- no digits found
       else Just (read digits, rest)  -- convert to int and return

-- <string> ::= <letter> | <letter> <string>
parseString :: String -> Maybe (String, String)
parseString [] = Just ("", [])
parseString (x:xs)
  | x == ' '  = case parseString xs of
      Just (rest, r) -> Just (' ' : rest, r)
      Nothing        -> Nothing
  | otherwise = case parseLetter (x:xs) of
      Just (firstLetter, rest) -> case parseString rest of
          Just (remainingString, finalRest) -> Just (firstLetter : remainingString, finalRest)
          Nothing -> Nothing
      Nothing -> Nothing

-- <movie_id> ::= <number>
parseMovieId :: String -> Maybe (Int, String)
parseMovieId input = parseNumber input

-- <rating> ::= <number> "." <number> | <number>
parseRating :: String -> Maybe (Double, String)
parseRating input = 
  case parseNumber input of
    Nothing -> Nothing
    Just (intPart, '.' : rest) ->  -- if number is decimal
      case parseNumber rest of
        Just (fracPart, rest') ->
          let rating = fromIntegral intPart + fromIntegral fracPart / 10
          in if rating >= 0.0 && rating <= 10.0
                then Just (rating, rest')
                else Nothing
        Nothing -> Nothing  -- if there is a point but no number after
    Just (intPart, rest) ->
      let rating = fromIntegral intPart
      in if rating >= 0.0 && rating <= 10.0
            then Just (rating, rest)
            else Nothing

-- <year> ::= <number>
parseYear :: String -> Maybe (Int, String)
parseYear input = parseNumber input

-- <director> ::= <string>
parseDirector :: String -> Maybe (String, String)
parseDirector input = parseString input

-- <genre> ::= <string>
parseGenre :: String -> Maybe (String, String)
parseGenre input = parseString input

-- <title> ::= <string>
parseTitle :: String -> Maybe (String, String)
parseTitle input = parseString input

-- <filter> ::= "all" | "genre:" <genre> | "year:" <year>
parseFilter :: String -> Maybe (String, String)
parseFilter input =
    case parseAll input of
        Just result -> Just result
        Nothing -> case parseGenreFilter input of
            Just result -> Just result
            Nothing -> parseYearFilter input

-- "all"
parseAll :: String -> Maybe (String, String)
parseAll ('a':'l':'l':rest) = Just ("all", rest)
parseAll _ = Nothing

-- "genre:" <genre>
parseGenreFilter :: String -> Maybe (String, String)
parseGenreFilter ('g':'e':'n':'r':'e':':':rest) =
    case parseGenre rest of
        Just (genre, remaining) -> case validateGenre genre of
            Right validGenre -> Just ("genre:" ++ validGenre, remaining)
            Left _ -> Nothing
        Nothing -> Nothing
parseGenreFilter _ = Nothing

-- "year:" <year>
parseYearFilter :: String -> Maybe (String, String)
parseYearFilter ('y':'e':'a':'r':':':rest) =
    case parseYear rest of
        Just (year, remaining) -> Just ("year:" ++ show year, remaining)  -- convert to string
        Nothing -> Nothing
parseYearFilter _ = Nothing

-- <add_movie> ::= "add_movie" "(" <title> "," <genre> "," <director> "," <year> ")"
parseAddMovie :: String -> Either String Query
parseAddMovie input =
  let cleanedContent = if head input == '(' && last input == ')'
                          then init (tail input)
                          else input
      parts = map trim (splitByComma cleanedContent)
  in case parts of
        [title, genre, director, yearStr] -> 
          case parseTitle title of
            Just (parsedTitle, _) -> 
              case validateGenre genre of
                Right validGenre ->
                  case parseDirector director of
                    Just (parsedDirector, _) ->
                      case parseYear yearStr of
                        Just (parsedYear, _) ->
                          Right (AddMovie parsedTitle validGenre parsedDirector parsedYear)
                        Nothing -> Left "Invalid year format"
                    Nothing -> Left "Invalid director format"
                Left err -> Left err
            Nothing -> Left "Invalid title format"
        _ -> Left "Invalid input format for add_movie. Expected: add_movie(<title>, <genre>, <director>, <year>)"


-- <rate_movie> ::= "rate_movie" "(" <movie_id> "," <rating> ")"
parseRateMovie :: String -> Either String Query
parseRateMovie input =
  let cleanedContent = if head input == '(' && last input == ')'
                          then init (tail input)
                          else input
      parts = map trim (splitByComma cleanedContent)
  in case parts of
        [movieIdStr, ratingStr] -> 
          case parseMovieId movieIdStr of
            Just (movieId, _) ->
              case parseRating ratingStr of
                Just (rating, _) -> Right (RateMovie movieId rating)
                Nothing -> Left "Invalid rating format or range. Must be a number between 0.0 and 10.0."
            Nothing -> Left "Invalid movie_id format. Expected a valid integer."
        _ -> Left "Invalid input format for rate_movie. Expected format: rate_movie(<movie_id>, <rating>)"

-- <update_movie> ::= "update_movie" "(" <movie_id> "," <title> "," <genre> "," <director> "," <year> ")"
parseUpdateMovie :: String -> Either String Query
parseUpdateMovie input =
  let cleanedContent = if head input == '(' && last input == ')'
                          then init (tail input)
                          else input
      parts = map trim (splitByComma cleanedContent)
  in case parts of
        [movieIdStr, titleStr, genreStr, directorStr, yearStr] -> 
          case parseMovieId movieIdStr of
            Just (movieId, _) ->
              let parseOrNothing str = if str == "_" then Nothing else Just str
                  title = parseOrNothing titleStr
                  genre = case parseOrNothing genreStr of
                            Just g -> case validateGenre g of
                                        Right validGenre -> Just validGenre
                                        Left _ -> Nothing
                            Nothing -> Nothing
                  director = parseOrNothing directorStr
                  year = if yearStr == "_" 
                         then Nothing 
                         else case parseYear yearStr of
                                Just (yearValue, _) -> Just yearValue
                                Nothing -> Nothing
              in Right (UpdateMovie movieId title genre director year)
            Nothing -> Left "Invalid movie_id format. Expected a valid integer."
        _ -> Left "Invalid input format for update_movie. Expected format: update_movie(<movie_id>, <title>, <genre>, <director>, <year>)"

-- <remove_movie> ::= "remove_movie" "(" <movie_id> ")"
parseRemoveMovie :: String -> Either String Query
parseRemoveMovie input =
  let cleanedContent = if head input == '(' && last input == ')'
                          then init (tail input)
                          else input
  in case parseMovieId cleanedContent of
        Just (movieId, _) -> Right (RemoveMovie movieId)
        Nothing -> Left "Invalid movie_id format. Expected a valid integer."

-- <view_movie_details> ::= "view_movie_details" "(" <movie_id> ")"
parseViewMovieDetails :: String -> Either String Query
parseViewMovieDetails input =
  let inputTrimmed = trim input
  in if head inputTrimmed == '(' && last inputTrimmed == ')'
     then case parseMovieId (init (tail inputTrimmed)) of
            Just (movieId, _) -> Right (ViewMovieDetails movieId)
            Nothing -> Left "Invalid movie ID"
     else Left "Invalid command format"

-- <list_movies> ::= "list_movies" "(" <filter> ")"
parseListMovies :: String -> Either String Query
parseListMovies input =
  let inputTrimmed = trim input
  in if head inputTrimmed == '(' && last inputTrimmed == ')'
     then case parseFilter (init (tail inputTrimmed)) of
            Just (filterStr, _) -> Right (ListMovies filterStr)
            Nothing -> Right (ListMovies "all")
     else Left "Invalid command format"


-- MANAGING MOVIES

showMovie :: Movie -> String
showMovie (Movie id title genre director year rating) =
  "(" ++ show id ++ ") \"" ++ title ++ "\" (" ++ genre ++ ", " ++ director ++ ", " ++ show year ++ ", " ++ show rating ++ ")"

lookupMovieById :: Int -> [Movie] -> Either String Movie
lookupMovieById _ [] = Left "Movie not found"
lookupMovieById movieId (movie@(Movie id _ _ _ _ _):rest)
  | movieId == id = Right movie
  | otherwise     = lookupMovieById movieId rest

updateMovieInList :: Int -> Movie -> [Movie] -> [Movie]
updateMovieInList _ _ [] = [] 
updateMovieInList targetId newMovie (x:xs)
  | movieId x == targetId = newMovie : xs 
  | otherwise = x : updateMovieInList targetId newMovie xs

removeMovieFromList :: Int -> [Movie] -> (Maybe String, [Movie])
removeMovieFromList _ [] = (Nothing, [])
removeMovieFromList targetId (movie:rest)
  | movieId movie == targetId = (Just "Movie removed", rest)
  | otherwise =
      let (result, remaining) = removeMovieFromList targetId rest
      in (result, movie : remaining)

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State
  { movies :: [Movie]      -- list of movies with their details (ID, Title, Genre, Director, Year, Rating)
  } deriving (Show)

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {movies = []}


processSequence :: State -> [Query] -> Either String (Maybe String, State)
processSequence st [] = Right (Nothing, st)
processSequence st (q:qs) =
  case stateTransition st q of
    Left err -> Left err
    Right (msg, newState) ->
      case processSequence newState qs of
        Left err -> Left err
        Right (msgs, finalState) ->
          let combinedMessages = case (msg, msgs) of
                                   (Nothing, Nothing) -> Nothing
                                   (Just m, Nothing) -> Just m
                                   (Nothing, Just ms) -> Just ms
                                   (Just m, Just ms) -> Just (m ++ "\n" ++ ms)
          in Right (combinedMessages, finalState)

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st (Sequence queries) = processSequence st queries
stateTransition st query = case query of

  AddMovie title genre director year ->
    let newId = length (movies st) + 1
        newRating = 0.0
        newMovie = Movie newId title genre director year newRating
        updatedMovies = newMovie : movies st
        newState = st { movies = updatedMovies }
    in Right (Just ("Added movie: " ++ showMovie newMovie), newState)

  ViewMovieDetails movieId ->
    case lookupMovieById movieId (movies st) of
      Right movie -> Right (Just (showMovie movie), st)
      Left err    -> Left err

  RateMovie movieId newRating -> 
    case lookupMovieById movieId (movies st) of
      Right (Movie id title genre director year _) ->
        let updatedMovie = Movie id title genre director year newRating
            updatedMovies = updateMovieInList movieId updatedMovie (movies st)
            newState = st { movies = updatedMovies }
        in Right (Just ("Updated movie rating: " ++ showMovie updatedMovie), newState)
      Left err -> Left err

  RemoveMovie movieId -> 
    let (result, remainingMovies) = removeMovieFromList movieId (movies st)
        newState = st { movies = remainingMovies }
    in case result of
        Just message -> Right (Just message, newState)
        Nothing -> Left "Movie not found"

  UpdateMovie movieId newTitle newGenre newDirector newYear ->
    case lookupMovieById movieId (movies st) of
      Right (Movie id title genre director year rating) ->
        let updatedMovie = Movie id
                                  (fromMaybe title newTitle)
                                  (fromMaybe genre newGenre)
                                  (fromMaybe director newDirector)
                                  (fromMaybe year newYear)
                                  rating
            updatedMovies = updateMovieInList movieId updatedMovie (movies st)
            newState = st { movies = updatedMovies }
        in Right (Just ("Updated movie: " ++ showMovie updatedMovie), newState)
      Left err -> Left err

  ListMovies "all" -> 
    let allMovies = movies st
        movieList = unlines (map showMovie allMovies)
        trimmedMovieList = reverse . dropWhile (== '\n') . reverse $ movieList
    in Right (Just ("All Movies:\n" ++ trimmedMovieList), st)

  ListMovies param -> 
    case L.stripPrefix "genre:" param of
      Just genreStr -> 
        let genreMovies = filter (\m -> case m of
                                          Movie _ _ g _ _ _ -> g == genreStr) (movies st)
            movieList = unlines (map showMovie genreMovies)
        in Right (Just ("Movies in genre " ++ genreStr ++ ":\n" ++ movieList), st)
      Nothing -> 
        case L.stripPrefix "year:" param of
          Just yearStr -> 
            case reads yearStr :: [(Int, String)] of
              [(year, "")] -> 
                let filteredMovies = filter (\m -> case m of
                                                     Movie _ _ _ _ y _ -> y == year) (movies st)
                    movieList = if null filteredMovies
                                then "No movies found from this year."
                                else unlines (map showMovie filteredMovies)
                in Right (Just ("Movies from year " ++ show year ++ ":\n" ++ movieList), st)
              _ -> Left "Invalid year format for list_movies"
          Nothing -> Left "Invalid parameter for list_movies"