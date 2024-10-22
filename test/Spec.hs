{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "Parsing empty string returns error" $
      Lib2.parseQuery "" @?= Left "Unknown command or invalid input",

    testCase "Parsing single character string returns error" $
      Lib2.parseQuery "o" @?= Left "Unknown command or invalid input",

    testCase "Parsing add_movie query" $
      Lib2.parseQuery "add_movie(The Matrix, SciFi, The Wachowskis, 1999)" 
        @?= Right (Lib2.AddMovie "The Matrix" "SciFi" "The Wachowskis" 1999),

    testCase "Parsing add_movie query with invalid genre" $
      Lib2.parseQuery "add_movie(The Matrix, Thriller, The Wachowskis, 1999)" 
        @?= Left "Invalid genre: Thriller. Available genres are: Action Comedy Drama Horror SciFi Romance",

    testCase "Parsing remove_movie query" $
      Lib2.parseQuery "remove_movie(1)" 
        @?= Right (Lib2.RemoveMovie 1),

    testCase "Parsing rate_movie query" $
      Lib2.parseQuery "rate_movie(1, 4.5)" 
        @?= Right (Lib2.RateMovie 1 4.5),

    testCase "Parsing update_movie query" $
      Lib2.parseQuery "update_movie(1, The Matrix Reloaded, SciFi, The Wachowskis, 2003)" 
        @?= Right (Lib2.UpdateMovie 1 (Just "The Matrix Reloaded") (Just "SciFi") (Just "The Wachowskis") (Just 2003)),

    testCase "Parsing view_movie_details query" $
      Lib2.parseQuery "view_movie_details(1)" 
        @?= Right (Lib2.ViewMovieDetails 1),

    testCase "Parsing list_movies query" $
      Lib2.parseQuery "list_movies(genre:SciFi)" 
        @?= Right (Lib2.ListMovies "genre:SciFi"),

    testCase "Parsing sequence of queries" $
      Lib2.parseQuery "add_movie(The Matrix, SciFi, The Wachowskis, 1999); rate_movie(1, 4.5); remove_movie(1)" 
        @?= Right (Lib2.Sequence 
          [ Lib2.AddMovie "The Matrix" "SciFi" "The Wachowskis" 1999
          , Lib2.RateMovie 1 4.5
          , Lib2.RemoveMovie 1
          ])
  ]