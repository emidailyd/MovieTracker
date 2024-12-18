{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Lib2
import qualified Lib3
import Data.Function (on)
import Data.List (sortBy)

main :: IO ()
main = defaultMain $ testGroup "Lib3 Tests"
    [ testProperty "marshallRenderRoundTrip" prop_marshallRenderRoundTrip
    , testProperty "marshallPreservesOrder" prop_marshallPreservesOrder
    ]

prop_marshallRenderRoundTrip :: Property
prop_marshallRenderRoundTrip = forAll genStateNonEmpty $ \state ->
    let statements = Lib3.marshallState state
        rendered = Lib3.renderStatements statements
    in case Lib3.parseStatements rendered of
        Right (parsedStatements, _) -> statementsAreEqual statements parsedStatements
        Left err -> error ("Parsing failed: " ++ err)

prop_marshallPreservesOrder :: Property
prop_marshallPreservesOrder = forAll genState $ \state ->
    let sortedMovies = sortBy (compare `on` Lib2.movieId) (Lib2.movies state)
        sortedState = Lib2.State sortedMovies
        unsortedStatements = Lib3.marshallState state
        sortedStatements = Lib3.marshallState sortedState
    in Lib3.renderStatements unsortedStatements == Lib3.renderStatements sortedStatements

genState :: Gen Lib2.State
genState = do
    movies <- listOf genMovie
    return (Lib2.State movies)

genStateNonEmpty :: Gen Lib2.State
genStateNonEmpty = do
    movies <- listOf1 genMovie
    return (Lib2.State movies)

validGenres :: [String]
validGenres = ["Action", "Comedy", "Drama", "Horror", "SciFi", "Romance"]

genString :: Gen String
genString = do
    len <- choose (1, 20)
    vectorOf len (elements ['a'..'z'])

genMovie :: Gen Lib2.Movie
genMovie = do
    movieId <- choose (1, 100)
    title <- genString
    genre <- elements validGenres
    director <- genString
    year <- choose (1888, 2024)
    rating <- genRating
    return (Lib2.Movie movieId title genre director year rating)

genRating :: Gen Double
genRating = do
    intPart <- choose (0 :: Int, 9 :: Int)
    decimalPart <- choose (0 :: Int, 9 :: Int)
    return (fromIntegral intPart + fromIntegral decimalPart / 10.0)

statementsAreEqual :: Lib3.Statements -> Lib3.Statements -> Bool
statementsAreEqual (Lib3.Batch q1) (Lib3.Batch q2) = q1 == q2
statementsAreEqual (Lib3.Single q1) (Lib3.Single q2) = q1 == q2
statementsAreEqual _ _ = False