module Main (main) where

import Yeet.Parser

main :: IO ()
main = do
  x <- getContents
  justParse x

justParse :: String -> IO ()
justParse inp = case parseYeet inp of
    Left err -> putStrLn err
    Right result -> print result
