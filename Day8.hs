module Main (main) where

import Data.Char (isDigit, toLower, isLetter)
import Data.List (nub)
import Data.Maybe (fromJust)

import Main.Utf8 (withUtf8)

-- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell

-- https://hackage.haskell.org/package/text-icu

import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize2

canonicalForm :: String -> String
canonicalForm s = T.unpack noAccents
  where
    noAccents = T.filter (not . property Diacritic) normalizedText
    normalizedText = normalize NFD (T.pack s)

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents 0

analyze  :: [String] -> Int -> String
analyze [] acc = show acc
analyze (x : xs) acc = analyze xs acc' where ch_long = length x
                                             cf = map toLower $ canonicalForm x
                                             acc' | ch_long >= 4 && ch_long <= 12 &&
                                                    any isDigit x &&
                                                    any (`elem` "aeiou") (filter isLetter cf) &&
                                                    any (`notElem` "aeiou") (filter isLetter cf) &&
                                                    (length $ nub cf) == ch_long = acc + 1
                                                  | otherwise = acc
