module Main (main) where

import qualified Data.Map as Map
import Data.List.Split (splitOn)

import Main.Utf8 (withUtf8)


main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    putStrLn $ unwords $ Map.keys $ Map.filter isGood $ analyze contents []

analyze  :: [String] -> [(String, [String])] -> Map.Map String [String]
analyze [] acc = Map.fromListWith (++) acc
analyze (x : xs) acc = analyze xs acc' 
                                        where inp = splitOn ": " x
                                              date = inp !! 0
                                              names = splitOn ", " (inp !! 1)
                                              acc' = acc ++ (zip names (repeat [date]))
                                              
isGood :: [String] -> Bool
isGood inp = let inp' = map (splitOn "-") inp
                 x = map (\x -> read x :: Int) $ map (\x -> x !! 0) $ inp'
                 y = map (\x -> read x :: Int) $ map (\x -> x !! 1) $ inp'
                 z = map (\x -> read x :: Int) $ map (\x -> x !! 2) $ inp'
                 t1 = if (all (<= 12) x) && (all (<= 31) y) then ["MDY"] else []
                 t2 = if (all (<= 31) x) && (all (<= 12) y) then ["DMY"] else []
                 t3 = if (all (<= 31) y) && (all (<= 12) z) then ["YDM"] else []
                 t4 = if (all (<= 12) y) && (all (<= 31) z) then ["YMD"] else []
                 t = t1 ++ t2 ++ t3 ++ t4
                        in ("11-09-01" `elem` inp && "DMY" `elem` t) ||
                           ("09-11-01" `elem` inp && "MDY" `elem` t) ||
                           ("01-09-11" `elem` inp && "YMD" `elem` t) ||
                           ("01-11-09" `elem` inp && "YDM" `elem` t)
