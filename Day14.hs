module Main (main) where

import Main.Utf8 (withUtf8)

import Data.Maybe (fromJust)
import Data.List (findIndex)

main :: IO ()

main = withUtf8 $ do
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents 0

analyze  :: [String] -> Double -> String
analyze [] acc = show acc
analyze (x : xs) acc = analyze xs acc' 
                                        where   jdigits = ['一', '二', '三', '四', '五', '六', '七', '八', '九']
                                                jdec = ['十', '百', '千']
                                                a = takeWhile (/= ' ') x
                                                b = drop 2 $ dropWhile (/= '×') x
                                                m1 = head $ reverse a
                                                m2 = head $ reverse b
                                                a' = reverse $ tail $ reverse a
                                                b' = reverse $ tail $ reverse b
                                                (a1, b1) = metrics a' jdigits jdec (0, 0)
                                                (a2, b2) = metrics b' jdigits jdec (0, 0)
                                                acc' = acc + (a1 + b1) * (a2 + b2) * (func m1) * (func m2)
                                        
metrics :: String -> String -> String -> (Double, Double) -> (Double, Double)
metrics [] jdigits jdec (acc1, acc2) = (acc1, acc2)
metrics [x] jdigits jdec (acc1, acc2) | x == '億' && (acc1 /= 0) = (0, acc2 + acc1 * 100000000)
                                      | x == '億' = (0, acc2 + 100000000)
                                      | x == '万' && (acc1 /= 0) = (0, acc2 + acc1 * 10000)
                                      | x == '万' = (0, acc2 + 10000)
                                      | x `elem` jdigits = (acc1 + (fromIntegral ((fromJust (findIndex (== x) jdigits))) + 1), acc2)
                                      | x `elem` jdec = let acc' = (fromIntegral (10 ^ ((fromJust (findIndex (== x) jdec)) + 1))) in (acc1 + acc', acc2)
metrics (x1 : x2 : xs) jdigits jdec (acc1, acc2) | x1 == '億' && (acc1 /= 0) = metrics (x2 : xs) jdigits jdec (0, acc2 + acc1 * 100000000)
                                                 | x1 == '億' = metrics (x2 : xs) jdigits jdec (0, acc2 + 100000000)
                                                 | x1 == '万' && (acc1 /= 0) = metrics (x2 : xs) jdigits jdec (0, acc2 + acc1 * 10000)
                                                 | x1 == '万' = metrics (x2 : xs) jdigits jdec (0, acc2 + 10000)
                                                 | (x2 == '万') && (x1 `elem` jdigits) = metrics xs jdigits jdec (0, acc2 + 10000 * (acc1 + fromIntegral (fromJust (findIndex (== x1) jdigits)) + 1))
                                                 | x2 == '万' = let acc' = fromIntegral $ (10 ^ ((fromJust (findIndex (== x1) jdec)) + 1)) in metrics xs jdigits jdec (0, acc2 + 10000 * (acc1 + acc'))
                                                 | (x2 == '億') && (x1 `elem` jdigits)  = metrics xs jdigits jdec (0, acc2 + 100000000 * (acc1 + fromIntegral (fromJust (findIndex (== x1) jdigits)) + 1))
                                                 | (x2 == '億') = let acc' = fromIntegral $ (10 ^ ((fromJust (findIndex (== x1) jdec)) + 1)) in metrics xs jdigits jdec (0, acc2 + 100000000 * (acc1 + acc'))
                                                 | (x1 `elem` jdigits) && (x2 `elem` jdec) = let acc' = fromIntegral $ ((fromJust (findIndex (== x1) jdigits)) + 1) * 10 ^ ((fromJust (findIndex (== x2) jdec)) + 1) in metrics xs jdigits jdec (acc1 + acc', acc2)
                                                 | x1 `elem` jdec = let acc' = fromIntegral $ (10 ^ ((fromJust (findIndex (== x1) jdec)) + 1)) in metrics (x2 : xs) jdigits jdec (acc1 + acc', acc2)
                                                                   
                        
func :: Char -> Double
func x  | x == '尺' = 10/33
        | x == '間' = 60/33
        | x == '丈' = 100/33
        | x == '町' = 3600/33
        | x == '里' = 129600/33
        | x == '毛' = 1/33000
        | x == '厘' = 1/3300
        | x == '分' = 1/330
        | x == '寸' = 1/33
        | otherwise = 1
