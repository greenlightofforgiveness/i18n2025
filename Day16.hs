{-
CP437:
https://wxmedit.github.io/

Idea — Codex (gpt-5.4), charToTile — Gemini
-}

module Main where

import Main.Utf8 (withUtf8)
import Data.Array (Array, (!), array)
import Data.Bits (xor)
import qualified Data.IntMap.Strict as IM
import Data.List (dropWhileEnd, foldl', nubBy)

main :: IO ()
main = withUtf8 $ do
    contents <- readFile "input.txt"
    printResult (solvePipes contents)

data Connection = None | Single | Double
    deriving (Eq, Show)

data Tile = Tile
    { north :: Connection
    , east :: Connection
    , south :: Connection
    , west :: Connection
    }
    deriving (Eq, Show)

emptyTile :: Tile
emptyTile = Tile None None None None

charToTile :: Char -> Tile
charToTile c = case c of
    '│' -> Tile Single None   Single None
    '─' -> Tile None   Single None   Single
    '┌' -> Tile None   Single Single None
    '┐' -> Tile None   None   Single Single
    '└' -> Tile Single Single None   None
    '┘' -> Tile Single None   None   Single
    '├' -> Tile Single Single Single None
    '┤' -> Tile Single None   Single Single
    '┬' -> Tile None   Single Single Single
    '┴' -> Tile Single Single None   Single
    '┼' -> Tile Single Single Single Single
    '║' -> Tile Double None   Double None
    '═' -> Tile None   Double None   Double
    '╔' -> Tile None   Double Double None
    '╗' -> Tile None   None   Double Double
    '╚' -> Tile Double Double None   None
    '╝' -> Tile Double None   None   Double
    '╠' -> Tile Double Double Double None
    '╣' -> Tile Double None   Double Double
    '╦' -> Tile None   Double Double Double
    '╩' -> Tile Double Double None   Double
    '╬' -> Tile Double Double Double Double
    '╥' -> Tile None   Single Double Single
    '╨' -> Tile Double Single None   Single
    '╡' -> Tile Single None   Single Double
    '╞' -> Tile Single Double Single None
    '╫' -> Tile Double Single Double Single
    '╪' -> Tile Single Double Single Double
    '╢' -> Tile Double None   Double Single
    '╟' -> Tile Double Single Double None
    '╧' -> Tile Single Double None   Double
    '╤' -> Tile None   Double Single Double
    _   -> emptyTile

rotateClockwise :: Tile -> Tile
rotateClockwise (Tile n e s w) = Tile w n e s

uniqueCandidates :: [(Tile, Int)] -> [(Tile, Int)]
uniqueCandidates = nubBy (\(tile1, _) (tile2, _) -> tile1 == tile2)

tileOptions :: Char -> [(Tile, Int)]
tileOptions ch
    | tile == emptyTile = [(emptyTile, 0)]
    | otherwise = uniqueCandidates [(t0, 0), (t1, 1), (t2, 2), (t3, 3)]
  where
    tile = charToTile ch
    t0 = tile
    t1 = rotateClockwise t0
    t2 = rotateClockwise t1
    t3 = rotateClockwise t2

connToDigit :: Connection -> Int
connToDigit None = 0
connToDigit Single = 1
connToDigit Double = 2

digitToConn :: Int -> Connection
digitToConn 0 = None
digitToConn 1 = Single
digitToConn 2 = Double
digitToConn _ = error "invalid connection digit"

getDigit :: Int -> Int -> Int
getDigit pos state = (state `div` (3 ^ pos)) `mod` 3

setDigit :: Int -> Int -> Int -> Int
setDigit pos value state = state + (value - current) * factor
  where
    factor = 3 ^ pos
    current = (state `div` factor) `mod` 3

normalizeRows :: String -> [String]
normalizeRows contents = map pad rows
  where
    rows = map (dropWhileEnd (== '\r')) (lines contents)
    width = maximum (0 : map length rows)
    pad row = row ++ replicate (width - length row) ' '

buildGrid :: [String] -> Array (Int, Int) [(Tile, Int)]
buildGrid rows = array bounds
    [ ((x, y), tileOptions ((rows !! y) !! x))
    | y <- [0 .. height - 1]
    , x <- [0 .. width - 1]
    ]
  where
    height = length rows
    width = length (head rows)
    bounds = ((0, 0), (width - 1, height - 1))

sourceOk :: Tile -> Bool
sourceOk tile = (north tile /= None) `xor` (west tile /= None)

sinkOk :: Tile -> Bool
sinkOk tile = (south tile /= None) `xor` (east tile /= None)

solvePipes :: String -> Maybe Int
solvePipes contents
    | null rows = Nothing
    | width == 0 = Nothing
    | otherwise = IM.lookup 0 finalDp
  where
    rows = normalizeRows contents
    height = length rows
    width = length (head rows)
    optionsGrid = buildGrid rows
    initialDp = IM.singleton 0 0
    finalDp = foldl' processColumn initialDp [0 .. width - 1]

    processColumn dp x = foldl' (advanceState x) IM.empty (IM.toList dp)

    advanceState x acc (inState, baseCost) = placeRow acc 0 None 0 baseCost
      where
        placeRow current y northReq outState totalCost
            | y == height = IM.insertWith min outState totalCost current
            | otherwise = foldl' tryCandidate current (optionsGrid ! (x, y))
          where
            isSource = x == 0 && y == 0
            isSink = x == width - 1 && y == height - 1
            westReq = digitToConn (getDigit y inState)

            tryCandidate next (tile, rotCost)
                | northMatches
                    && westMatches
                    && sourceMatches
                    && bottomMatches
                    && rightMatches
                    && sinkMatches =
                        placeRow
                            next
                            (y + 1)
                            (south tile)
                            nextOutState
                            (totalCost + rotCost)
                | otherwise = next
              where
                northMatches
                    | y == 0 = isSource || north tile == None
                    | otherwise = north tile == northReq

                westMatches
                    | x == 0 = isSource || west tile == None
                    | otherwise = west tile == westReq

                sourceMatches = not isSource || sourceOk tile

                bottomMatches
                    | y == height - 1 = isSink || south tile == None
                    | otherwise = True

                rightMatches
                    | x == width - 1 = isSink || east tile == None
                    | otherwise = True

                sinkMatches = not isSink || sinkOk tile

                nextDigit
                    | x == width - 1 = 0
                    | otherwise = connToDigit (east tile)

                nextOutState = setDigit y nextDigit outState

printResult :: Maybe Int -> IO ()
printResult Nothing = putStrLn "error"
printResult (Just steps) = print steps
