module Main where

import Lib

main :: IO ()
main = do
    board <- getBoard
    play board
