module Main (main) where

import Main.Utf8 (withUtf8)

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Char (toUpper, isPunctuation)

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    let names = map (map toUpper) ["Οδυσσευς", "Οδυσσεως", "Οδυσσει", "Οδυσσεα", "Οδυσσευ"]
    putStrLn $ analyze contents names 0

analyze  :: [String] -> [String] -> Int -> String
analyze [] names acc = show acc
analyze (x : xs) names acc = analyze xs names acc' 
                                        where acc' = acc + (func (map toUpper (filter (not . isPunctuation) x)) names 1)

func :: String -> [String] -> Int -> Int
func x names i  | i == 24 = 0
                | any (== True) $ map (\w -> w `elem` names) x' = i
                | otherwise = func x names (i + 1)
                        where x' = map (\x -> cypher x i) (words x)

cypher :: String -> Int -> String
cypher x i = map (\c -> shift c "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ" i) x

shift :: Char -> String -> Int -> Char
shift c alphabet i | c `notElem` alphabet = c
                   | otherwise = let i' = ((fromJust (findIndex (== c) alphabet)) + i) `mod` 24 in alphabet !! i'