module Main (main) where

import Main.Utf8 (withUtf8)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Char (toLower, isLatin1, isLetter)

import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import qualified Data.ByteString.Base16 as B16

import Data.Text.Encoding.Error (lenientDecode)

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    let dict = takeWhile (/= "") contents
    let crossword = map (filter (/= ' ')) $ map (map toLower) $ tail $ dropWhile (/= "") contents
    let contents' = map deleteBOM contents
    let dict1 = map (\x -> map (toLower) $ T.unpack $ E.decodeUtf8With lenientDecode $ B16.decodeLenient $ B.pack x) contents'
    let dict2 = map (\x -> map (toLower) $ T.unpack $ E.decodeUtf16LEWith lenientDecode $ B16.decodeLenient $ B.pack x) contents'
    let dict3 = map (\x -> map (toLower) $ T.unpack $ E.decodeUtf16BEWith lenientDecode $ B16.decodeLenient $ B.pack x) contents'
    let dict4 = map (\x -> if ((all isLatin1) (B.unpack $ B16.decodeLenient $ B.pack x)) then map toLower $ T.unpack $ E.decodeLatin1 $ B16.decodeLenient $ B.pack x else "") contents'
    putStrLn $ analyze crossword dict1 dict2 dict3 dict4 0

analyze  :: [String] -> [String] -> [String] -> [String] -> [String] -> Int -> String
analyze [] _ _ _ _ acc = show acc
analyze (p : ps) dict1 dict2 dict3 dict4 acc = analyze ps dict1 dict2 dict3 dict4 acc'
                                                where   len = length p
                                                        i = fromJust $ findIndex (/= '.') p
                                                        word1 = findIndex (\w -> (length w == len) && ((w !! i) == (p !! i)) && (all isLetter w)) dict1
                                                        word2 = findIndex (\w -> (length w == len) && ((w !! i) == (p !! i)) && (all isLetter w)) dict2
                                                        word3 = findIndex (\w -> (length w == len) && ((w !! i) == (p !! i)) && (all isLetter w)) dict3
                                                        word4 = findIndex (\w -> (length w == len) && ((w !! i) == (p !! i)) && (all isLetter w)) dict4
                                                        ans = fromJust $ (filter (/= Nothing) [word1, word2, word3, word4]) !! 0
                                                        acc' = acc + ans + 1
                                                                                                
deleteBOM :: String -> String
deleteBOM x | take 6 x == "efbbbf" = drop 6 x
            | take 4 x == "fffe" =  drop 4 x
            | take 4 x == "feff" = drop 4 x
            | otherwise = x