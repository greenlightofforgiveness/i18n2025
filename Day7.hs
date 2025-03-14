module Main (main) where

import qualified Data.Time.Clock as DTC
import qualified Data.Time.ISO8601 as ISO
import Data.Maybe (fromJust)

import qualified Data.ByteString.UTF8 as UTF8
import Data.Time
import Data.Time.Zones (localTimeToUTCTZ, utcToLocalTimeTZ)
import Data.Time.Zones.All (tzByName)

import Main.Utf8 (withUtf8)

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents 1 0

analyze  :: [String] -> Int -> Int -> String
analyze [] i acc = show acc
analyze (x : xs) i acc = analyze xs (i + 1) (acc + ans'')
                        where inp_data = words x
                              time = fromJust $ ISO.parseISO8601 (inp_data !! 0)
                              local_time = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%s%Q" (take 23 $ (inp_data !! 0)) :: LocalTime
                              time_Halifax = localTimeToUTCTZ (fromJust $ tzByName (UTF8.fromString "America/Halifax")) local_time
                              time_Santiago = localTimeToUTCTZ (fromJust $ tzByName (UTF8.fromString "America/Santiago")) local_time
                              x1 = read (inp_data !! 1) :: Integer
                              x2 = read (inp_data !! 2) :: Integer
                              ans = DTC.addUTCTime (fromInteger (x1 * 60)) (DTC.addUTCTime (fromInteger ((-x2) * 60)) time)
                              ans' | time_Halifax == time = utcToLocalTimeTZ (fromJust $ tzByName (UTF8.fromString "America/Halifax")) ans
                                   | otherwise = utcToLocalTimeTZ (fromJust $ tzByName (UTF8.fromString "America/Santiago")) ans
                              ans'' = (read (take 2 $ drop 11 $ show ans') :: Int) * i
