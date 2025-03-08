module Main (main) where

import qualified Data.Time.Clock as DTC
import qualified Data.Time.ISO8601 as ISO
import Data.Maybe (fromJust)
import qualified Data.Map as Map

main :: IO ()

main = do
    contents <- map fromJust . map ISO.parseISO8601 . lines <$> readFile "input.txt"
    putStrLn $ analyze contents

analyze  :: [DTC.UTCTime] -> String
analyze times = ans' ++ "+00:00"
                        where acc = filter (\(a, b) -> b >= 4) $ Map.toList $ Map.fromListWith (+) $ zip times (repeat 1)
                              ans = ISO.formatISO8601 $ fst (acc !! 0)
                              ans' = reverse . tail . reverse $ ans
