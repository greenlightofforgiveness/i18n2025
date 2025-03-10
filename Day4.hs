module Main (main) where

import qualified Data.ByteString.UTF8 as UTF8
import Data.Time
import Data.Time.Zones (TZ, localTimeToUTCTZ)
import Data.Time.Zones.All (tzByName)
import Data.Maybe (fromJust)

-- https://stackoverflow.com/questions/4174372/haskell-date-parsing-and-formatting

main :: IO ()

main = do
    contents <- filter (/= "") . lines <$> readFile "input.txt"
    putStrLn $ analyze contents 0

analyze  :: [String] -> NominalDiffTime -> String
analyze [] acc = show $ (read (reverse . tail . reverse . show $ acc) :: Int) `div` 60
analyze (x1 :  x2 : xs) acc = analyze xs acc'
                                        where   data1 = words x1
                                                data2 = words x2
                                                dateString1 = unwords $ drop 2 data1
                                                dateString2 = unwords $ drop 2 data2
                                                timeFromString1 = parseTimeOrError True defaultTimeLocale "%b %d, %Y, %H:%M" dateString1 :: LocalTime
                                                timeFromString2 = parseTimeOrError True defaultTimeLocale "%b %d, %Y, %H:%M" dateString2 :: LocalTime
                                                tz1 = fromJust $ tzByName $ UTF8.fromString (data1 !! 1)
                                                tz2 = fromJust $ tzByName $ UTF8.fromString (data2 !! 1)
                                                t1 = localTimeToUTCTZ tz1 timeFromString1
                                                t2 = localTimeToUTCTZ tz2 timeFromString2
                                                acc' = acc + (diffUTCTime t2 t1)
