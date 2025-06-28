module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["src/Scanner.hs", "src/Token.hs", "src/Scanner"]
