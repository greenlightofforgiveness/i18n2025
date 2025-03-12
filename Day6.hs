module Main (main) where

import Main.Utf8 (withUtf8)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Char (toLower)

import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    let dict = takeWhile (/= "") contents
    let crossword = map (map toLower) $ tail $ dropWhile (/= "") contents
    let dict' = map (map toLower) $ decode' dict 1 []
    putStrLn $ analyze crossword dict' 0

analyze  :: [String] -> [String] -> Int -> String
analyze [] dict acc = show acc
analyze (c : cs) dict acc = analyze cs dict acc'
                                        where   ptrn = filter (/= ' ') c
                                                len = length ptrn
                                                i = fromJust $ findIndex (/= '.') ptrn
                                                word = findIndex (\w -> (length w == len) && ((w !! i) == (ptrn !! i))) dict
                                                acc' = acc + (fromJust word) + 1
                                                
decode' :: [String] -> Int -> [String] -> [String]
decode' [] _ acc = acc
decode' (x : xs) i acc = decode' xs (i + 1) acc' where acc' | i `mod` 15 == 0 = acc ++ [T.unpack $ E.decodeUtf8 $ B.pack $ T.unpack $ E.decodeUtf8 $ B.pack x]
                                                            | (i `mod` 3 == 0) || (i `mod` 5 == 0) = acc ++ [T.unpack $ E.decodeUtf8 $ B.pack x]
                                                            | otherwise = acc ++ [x]