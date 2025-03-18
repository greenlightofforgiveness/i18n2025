module Main (main) where

import Main.Utf8 (withUtf8)

import Data.List.Split (splitOn)
import Data.Sort (sortOn)
import Data.Char (toLower, isLetter, ord)
import Data.Maybe (fromJust)
import qualified Data.List as L

import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize2

-- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell

canonicalForm :: String -> String
canonicalForm s = T.unpack noAccents
  where
    noAccents = T.filter (not . property Diacritic) normalizedText
    normalizedText = normalize NFD (T.pack s)
    

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    let inp = map (\x -> let [a, b] = splitOn ": " x in (takeWhile (/= ',') a, (read b :: Int))) contents
    putStrLn $ show $ analyze inp

analyze :: [(String, Int)] -> Int
analyze inp = let sortEng = sortOn fst $ map (\(a, b) -> ((canonicalForm $ (\x -> replaceEng x []) $ map toLower $ filter isLetter a), b)) inp
                  i = (length inp) `div` 2
                  ansEng = snd (sortEng !! i)
                  alphabetSwed = "abcdefghijklmnopqrstuvwxyzåäö"
                  sortSwed = sortOn fst $ map (\(a, b) -> (map (\c -> if (c `elem` alphabetSwed) then fromJust $ L.findIndex (== c) alphabetSwed else ord c) $ (\x -> replaceSwed x []) $ map toLower $ filter isLetter a, b)) inp
                  ansSwed = snd (sortSwed !! i)
                  sortDutch = sortOn fst $ map (\(a, b) -> ((canonicalForm $ (\x -> replaceEng x []) $ map toLower $ filter isLetter $ (unwords . filter (`notElem` ["van", "den", "de", "der"]). words) a), b)) inp
                  ansDutch = snd (sortDutch !! i)
                        in ansSwed * ansEng * ansDutch

replaceEng :: String -> String -> String            
replaceEng [] acc = acc            
replaceEng (c : cs) acc | c == 'æ' = replaceEng cs (acc ++ "ae")
                        | c == 'ø' = replaceEng cs (acc ++ "o")
                        | otherwise = replaceEng cs (acc ++ [c])
                         
replaceSwed :: String -> String -> String            
replaceSwed [] acc = acc            
replaceSwed (c : cs) acc | c == 'æ' = replaceSwed cs (acc ++ "ä")
                         | c == 'ø' = replaceSwed cs (acc ++ "ö")
                         | c `elem` alphabetSwed = replaceSwed cs (acc ++ [c])
                         | otherwise = replaceSwed cs (acc ++ canonicalForm [c])
                                where alphabetSwed = "abcdefghijklmnopqrstuvwxyzåäö"
