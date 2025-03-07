module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

import Main.Utf8 (withUtf8)

-- https://hackage.haskell.org/package/with-utf8

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents 0
     
analyze [] acc = show acc
analyze (x : xs) acc = analyze xs acc' where bytes_long = BS.length $ UTF8.fromString x
                                             ch_long = length x
                                             acc' | bytes_long <= 160 && ch_long <= 140 = acc + 13
                                                  | bytes_long <= 160 = acc + 11
                                                  | ch_long <= 140 = acc + 7
                                                  | otherwise = acc