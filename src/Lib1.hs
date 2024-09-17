module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [
        "add_movie",               -- Add a movie to the database
        "rate_movie",              -- Rate an existing movie
        "update_movie",            -- Update details of a movie
        "remove_movie",            -- Remove a movie from the list
        "list_movies",             -- List movies, optionally filtered
        "view_movie_details",      -- View details of a specific movie
        "genre:",                  -- Filter by genre
        "year:",                   -- Filter by year
        "Action",                  -- Example genre
        "Drama",                   -- Example genre
        "Comedy",                  -- Example genre
        "Horror",                  -- Example genre
        "SciFi",                   -- Example genre
        "Romance"                  -- Example genre
    ]

