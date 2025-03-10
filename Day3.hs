module Main (main) where

import Data.Char (isDigit, isUpper, isLower, isAscii)

import Main.Utf8 (withUtf8)

-- https://hackage.haskell.org/package/with-utf8

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents 0

analyze  :: [String] -> Int -> String
analyze [] acc = show acc
analyze (x : xs) acc = analyze xs acc' where ch_long = length x
                                             acc' | ch_long >= 4 && ch_long <= 12 && any isDigit x && any isUpper x && any isLower x && any (not . isAscii) x = acc + 1
                                                  | otherwise = acc