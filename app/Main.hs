module Main where

import Lib
import System.Console.ANSI (setSGR, SGR (Reset))


main :: IO ()
main = do
    setSGR [Reset]
    board <- getBoard
    play board
