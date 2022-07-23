module Lib where

import Data.Char

data Color
    = Grey
    | LightBlue
    | Red
    | Yellow
    | Brown
    | Orange
    | DarkBlue
    | Pink
    | Purple
    | LightGreen
    | Green
    | DarkGreen
    deriving (Read, Show, Enum, Eq, Ord)

type Board = [Tube]

type Tube = ([Color], Int)

isFull :: Tube -> Bool
isFull tube@(colors, index) = length colors == 4

freeSpace :: Tube -> Int
freeSpace tube@(colors, index) = 4 - length colors

pourSize :: Tube -> Int
pourSize ([], index) = 0
pourSize tube@(colors, index) = length (takeWhile (== head colors) colors)

canPour :: Tube -> Tube -> Bool
canPour t1@(colors1, i1) t2@(colors2, i2)
    | isFull t2 = False
    | null colors1 = False
    | null colors2 = True
    | let neededSpace = pourSize t1
          space = freeSpace t2
       in head colors1 == head colors2 && neededSpace <= space = True
    | otherwise = False

move :: Tube -> Tube -> (Tube, Tube)
move t1@(c1, i1) t2@(c2, i2) =
    if canPour t1 t2
        then ( (dropWhile (== head c1) c1, i1)
             , (takeWhile (== head c1) c1 ++ c2, i2))
        else (t1, t2)

pour :: Board -> Int -> Int -> (Bool, Board)
pour board i1 i2
    | i1 >= length board || i2 >= length board || i1 < 0 || i2 < 0 =
        (False, board)
    | not (canPour t1 t2) = (False, board)
    | otherwise =
        let (newTube1, newTube2) = move t1 t2
         in (True, map (replaceTubes t1 t2 newTube1 newTube2) board)
  where
    t1 = board !! i1
    t2 = board !! i2

replaceTubes :: Tube -> Tube -> Tube -> Tube -> Tube -> Tube
replaceTubes t1 t2 nt1 nt2 currentTube
    | currentTube == t1 = nt1
    | currentTube == t2 = nt2
    | otherwise = currentTube

getBoard board = do
    input <- getLine
    if null input
        then putStrLn "done"
        else do
            let newColor = getColor input
                newBoard = board ++ [newColor]
            print newBoard
            getBoard newBoard

getColor x =
    case map toLower x of
        "red" -> Red
        "grey" -> Grey
        "lightblue" -> LightBlue
        "yellow" -> Yellow
        "brown" -> Brown
        "orange" -> Orange
        "darkblue" -> DarkBlue
        "pink" -> Pink
        "purple" -> Purple
        "lightgreen" -> LightGreen
        "green" -> Green
        "darkGreen" -> DarkGreen
        _ -> undefined

getTube =
    let fn' colors = do
            input <- getLine
            if null input
                then colors
                else do
                    newColor <- getColor input
                    let newColors = colors ++ [newColor]
                    fn' newColors
     in fn' []
