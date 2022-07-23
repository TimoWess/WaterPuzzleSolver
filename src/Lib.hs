module Lib where

import Control.Monad
import Data.Char
import Foreign (moveBytes)
import GHC.Base (VecElem(Int16ElemRep))
import System.IO
import System.Random

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

type Move = (Int, Int)

maxFails = 10000

isFull :: Tube -> Bool
isFull tube@(colors, index) = length colors == 4

freeSpace :: Tube -> Int
freeSpace tube@(colors, index) = 4 - length colors

pourSize :: Tube -> Int
pourSize ([], index) = 0
pourSize tube@(colors, index) = length (takeWhile (== head colors) colors)

canPour :: Tube -> Tube -> Bool
canPour t1@(colors1, i1) t2@(colors2, i2)
    | i1 == i2 = False
    | isFull t2 = False
    | null colors1 = False
    | pourSize t1 == length colors1 && null colors2 = False
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

getBoard :: IO Board
getBoard = fn' []
  where
    fn' board = do
        putStr "Add a new tube? [y/n] "
        hFlush stdout
        input <- getLine
        if null input || input == "n"
            then return (reverse board)
            else do
                newTube <- getTube
                let newBoard = (newTube, length board) : board
                print newBoard
                fn' newBoard

getTube :: IO [Color]
getTube = fn' []
  where
    fn' colors = do
        putStr "Add a color: "
        hFlush stdout
        input <- getLine
        if null input
            then return colors
            else do
                let newColor = getColor input
                case newColor of
                    Just val -> do
                        let newColors = colors ++ [val]
                        print newColors
                        fn' newColors
                    Nothing -> do
                        putStrLn "Invalid color. Try again!"
                        fn' colors

getColor :: String -> Maybe Color
getColor x =
    case map toLower x of
        "red" -> Just Red
        "grey" -> Just Grey
        "lightblue" -> Just LightBlue
        "yellow" -> Just Yellow
        "brown" -> Just Brown
        "orange" -> Just Orange
        "darkblue" -> Just DarkBlue
        "pink" -> Just Pink
        "purple" -> Just Purple
        "lightgreen" -> Just LightGreen
        "green" -> Just Green
        "darkgreen" -> Just DarkGreen
        _ -> Nothing

leftPad :: String -> Int -> Char -> String
leftPad str num chr = replicate (num - length str) chr ++ str

printBoard :: Board -> IO ()
printBoard board =
    forM_ board $ \i -> do
        let (colors, index) = i
            maxLength = length (show $ snd $ last board)
        putStrLn $ leftPad (show index) maxLength '0' ++ ": " ++ show colors

play :: Board -> IO ()
play board = do
    putStr
        "Move, add a tube, print board, enable autoplay, clear board or quit? [m/a/p/auto/c/q] "
    hFlush stdout
    input <- getLine
    case map toLower input of
        "p" -> do
            printBoard board
            play board
        "m" -> do
            putStr "Select pouring tube: "
            hFlush stdout
            input <- getLine
            let i1 = read input :: Int
            putStr "Select revieving tube: "
            hFlush stdout
            input <- getLine
            let i2 = read input :: Int
                (success, newBoard) = pour board i1 i2
            print newBoard
            play newBoard
        "a" -> do
            newTube <- getTube
            let newBoard = reverse ((newTube, length board) : reverse board)
            play newBoard
        "c" -> do
            play []
        "q" -> do
            putStrLn "Good game!"
            return ()
        "auto" -> do
            let sub times = do
                    (newBoard, moves, moveList, success) <- autoPlay board
                    if not success
                        then do
                            putStrLn $
                                "Try " ++ show times ++ " failed. Trying again!"
                            sub (times + 1)
                        else do
                            putStrLn $ show moves ++ " moves needed!"
                            let options = do
                                    putStr
                                        "Show move list, continue or new game? [s/c/n] "
                                    hFlush stdout
                                    input <- getLine
                                    case map toLower input of
                                        "s" -> do
                                            putStrLn $
                                                replicate
                                                    (max (length $ show moveList)
                                                         20)
                                                    '='
                                            putStrLn "Before:"
                                            printBoard board
                                            putStrLn $
                                                replicate
                                                    (max (length $ show moveList)
                                                         20)
                                                    '='
                                            print moveList
                                            putStrLn $
                                                replicate
                                                    (max (length $ show moveList)
                                                         20)
                                                    '='
                                            putStrLn "After:"
                                            printBoard newBoard
                                            putStrLn $
                                                replicate
                                                    (max (length $ show moveList)
                                                         20)
                                                    '='
                                            play newBoard
                                        "c" -> play newBoard
                                        "n" -> play board
                                        _ -> do
                                            putStrLn "Invalid input!"
                                            options
                            options
            sub 1
        _ -> do
            putStrLn "Invalid input. Try again!"
            play board

autoPlay :: Board -> IO (Board, Integer, [Move], Bool)
autoPlay board = fn' board 0 0 []
  where
    fn' :: Board
        -> Integer
        -> Integer
        -> [Move]
        -> IO (Board, Integer, [Move], Bool)
    fn' board moves fails moveList = do
        if isDone board
            then return (board, moves, moveList, True)
            else if fails >= maxFails
                     then return (board, moves, moveList, False)
                     else do
                         r1 <- randomRIO (0, length board - 1)
                         r2 <- randomRIO (0, length board - 1)
                         let (success, newBoard) = pour board r1 r2
                         if success
                             then fn' newBoard
                                      (moves + 1)
                                      0
                                      ((r1, r2) : moveList)
                             else fn' board moves (fails + 1) moveList

isDone :: Board -> Bool
isDone = all finished

finished :: Tube -> Bool
finished tube@(colors, index)
    | null colors = True
    | length colors < 4 = False
    | otherwise =
        let needed = head colors
         in all (== needed) colors
