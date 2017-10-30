module Main where

import Data.Array
import System.IO

type Mark = Char
type Bammen = Array (Int, Int) Char

main :: IO ()
main = do
    let bammen = listArray ((0, 0), (2, 2)) [ ' ', ' ', ' '
                                            , ' ', ' ', ' '
                                            , ' ', ' ', ' '
                                            ]
    printBammen bammen
    gameLoop 9 bammen

gameLoop :: Int -> Bammen -> IO ()
gameLoop 0 _      = return ()
gameLoop n bammen = do
    let mark = if n `mod` 2 == 0 then 'x' else 'o'
    putStrLn $ mark : " の番です"
    pos <- inputLoop bammen
    let bammen' = bammen // [(pos, mark)]
    printBammen bammen'
    if doesWin mark bammen'
        then putStrLn $ mark : " の勝ち"
        else gameLoop (n - 1) bammen'

inputLoop :: Bammen -> IO (Int, Int)
inputLoop bammen = do
    putStr $ "X 座標: "
    hFlush stdout
    x <- readLn
    putStr $ "Y 座標: "
    hFlush stdout
    y <- readLn
    let pos = (x, y)
    if bammen ! pos == ' '
        then return pos
        else do
            putStrLn "そこには置けません"
            inputLoop bammen

printBammen :: Bammen -> IO ()
printBammen bammen = do
    putStrLn "  0   1   2"
    putStrLn $ foldr go "" $ assocs bammen
    where
        go ((2, 2), mark) acc = mark : '\n' : acc
        go ((n, 0), mark) acc = (show n) ++ ' ' : mark : " | " ++ acc
        go ((_, 1), mark) acc = mark : " | " ++ acc
        go ((_, 2), mark) acc = mark : "\n  --+---+--\n" ++ acc
        go _              _   = error "wrong case"

doesWin :: Mark -> Bammen -> Bool
doesWin mark bammen =
    any (all ((== mark) . (bammen !))) [ [(0, 0), (0, 1), (0, 2)]
                                       , [(1, 0), (1, 1), (1, 2)]
                                       , [(2, 0), (2, 1), (2, 2)]
                                       , [(0, 0), (1, 0), (2, 0)]
                                       , [(1, 0), (1, 1), (1, 2)]
                                       , [(2, 0), (2, 1), (2, 2)]
                                       , [(0, 0), (1, 1), (2, 2)]
                                       , [(0, 2), (1, 1), (2, 0)]
                                       ]
