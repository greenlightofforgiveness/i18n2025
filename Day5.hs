module Main (main) where

import Main.Utf8 (withUtf8)

main :: IO ()

main =  withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents 0 0

analyze  :: [String] -> Int -> Int -> String
analyze [] i acc = show acc
analyze (x : xs) i acc = analyze xs i' acc'
                                        where acc' = if x !! i == '💩' then acc + 1 else acc
                                              i' = (i + 2) `mod` (length x)