module Main (main) where

import Main.Utf8 (withUtf8)

import Crypto.BCrypt

import qualified Data.Map as Map
import Data.Maybe (fromJust)

import qualified Data.ByteString.UTF8 as B
import qualified Data.Text.Encoding as E

import Data.Char (isMark)
import Unicode.Char.Normalization (compose)

import qualified Data.Text as T
import Data.Text.ICU.Normalize2

canonicalForm :: String -> String
canonicalForm s = T.unpack $ normalize NFD (T.pack s)

generateForms str i acc | (i == (length str) - 2) && (not $ isMark (str !! (i + 1))) = map (\x -> x ++ [str !! i] ++ [str !! (i + 1)]) acc
                        | (i == (length str) - 2) && (isMark (str !! (i + 1))) = ((map (\x -> x ++ [fromJust (compose (str !! i) (str !! (i + 1)))]) acc) ++ map (\x -> x ++ [str !! i] ++ [str !! (i + 1)]) acc)     
                        | i == (length str) - 1 = map (\x -> x ++ [str !! i]) acc
                        | isMark (str !! (i + 1)) = generateForms str (i + 2) ((map (\x -> x ++ [fromJust (compose (str !! i) (str !! (i + 1)))]) acc) ++ map (\x -> x ++ [str !! i] ++ [str !! (i + 1)]) acc)
                        | otherwise = generateForms str (i + 1) (map (\x -> x ++ [str !! i]) acc)

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    let part1 = map words $ takeWhile (/= "") contents
    let part1' = Map.fromList $ map (\x -> (x !! 0, x !! 1)) part1
    let part2 = map words $ tail $ dropWhile (/= "") contents
    putStrLn $ analyze part1' part2 0 (Map.fromList [])

analyze :: Map.Map String String -> [[String]] -> Int -> Map.Map String Bool -> String
analyze p1 [] acc m = show acc    
analyze p1 ([name, pass] : ps) acc m = analyze p1 ps acc' m' where pass_forms_check | Map.lookup pass m /= Nothing = (m Map.! pass)
                                                                                    | otherwise = let pass_forms = (generateForms (canonicalForm pass) 0 [""]) in
                                                                                                      any (== True) $ map (\p -> validatePassword (B.fromString $ T.unpack $ E.decodeUtf8 $ B.fromString (p1 Map.! name)) (B.fromString $ T.unpack $ E.decodeUtf8 $ B.fromString p)) pass_forms
                                                                   acc' | pass_forms_check == True = acc + 1
                                                                        | otherwise = acc
                                                                   m' = if (Map.lookup pass m /= Nothing) then m else (Map.insert pass pass_forms_check m)