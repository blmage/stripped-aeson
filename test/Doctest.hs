module Main where

import Test.DocTest


main :: IO ()
main = doctest [ "-isrc", "src/Deriving/Aeson/Stripped.hs" ]
