module Tictactoe
where

import Data.Char
import Data.List

data Player = O | B | X deriving (Eq,Ord,Show)  -- ( O < B < X)

type Board = [[Player]]

next :: Player -> Player
next O = X 
next B = B 
next X = O

empty :: Board
empty = replicate 3 (replicate 3 B)

full :: Board -> Bool
full board = all (/= B) (concat board)

wins :: Player -> Board -> Bool
wins player board = any line (rows ++ cols ++ dias) --[[x,x,x],[x,o,x],[o,x,o].....] if any of these is true
                    where
                        line = all (== player)
                        rows = board
                        cols = transpose board
                        dias = [diag board, diag (map reverse board)]

diag :: Board -> [Player]
diag board = [board !! n !! n | n <- [0..2]]

won :: Board -> Int
won board 
    | wins O board = -1
    | wins X board = 1
    | otherwise =  0

valid :: Board -> Int -> Bool
valid board index = 0 <= index && index < 9 && concat board !! index == B

move :: Board -> Int -> Player -> [Board]
move board index player =
    if valid board index then [chop 3 (xs ++ [player] ++ ys)] else []
    where (xs, B:ys) = splitAt index (concat board)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)


pickMove :: Board -> Player -> Board
pickMove board player 
    | x1 == 1 && player == X = move1
    | x2 == -1 && player == X = block1
    | o1 == -1 && player == O = move3
    | o2 == 1 && player == O = block2
    | centerControl = concat $ move board 4 player
    | extraControl = concat $ move board moveId' player
    | cornersControl = concat $ move board moveId player
    | otherwise = move1


        where
        movesPlayer = moves board player
        movesOpponent = moves board (next player)

        gameStatusPlayer = [(won move, move) | move <- movesPlayer]
        gameStatusOpponent = [(won move, move) | move <- movesOpponent]

        (x1,move1) = maximum gameStatusPlayer
        (x2,move2) = minimum gameStatusOpponent

        (o1,move3) = minimum gameStatusPlayer
        (o2,move4) = maximum gameStatusOpponent

        block1 = replaceMove board move2 [] player
        block2 = replaceMove board move4 [] player

        centerControl = controlCenter board
        (extraControl, moveId') = extraCondition board
        (cornersControl, moveId) = controlCorners board

replaceMove :: Board -> Board -> Board -> Player -> Board
replaceMove [] _ acc _ = acc
replaceMove (h:rest) (h2:rest2) acc p = if h == h2 then replaceMove rest rest2 (acc++[h]) p
                                    else case h of
                                        [a,b,c] -> case h2 of
                                                        [a2,b2,c2] -> if a /= a2 then replaceMove rest rest2 (acc ++ [[p,b,c]]) p
                                                                            else if b /= b2 then replaceMove rest rest2 (acc ++ [[a,p,c]]) p
                                                                                else replaceMove rest rest2 (acc ++ [[a,b,p]]) p

controlCenter :: Board -> Bool
controlCenter [[_,_,_],[_,B,_],[_,_,_]] = True 
controlCenter _ = False

controlCorners :: Board -> (Bool,Int)
controlCorners [[_,B,B],[B,_,B],[B,B,_]] = (True,1)
controlCorners [[B,B,_],[B,_,B],[_,B,B]] = (True,1)
controlCorners [[B,_,_],[_,_,_],[_,_,_]] = (True,0)
controlCorners [[_,_,B],[_,_,_],[_,_,_]] = (True,2)
controlCorners [[_,_,_],[_,_,_],[B,_,_]] = (True,6)
controlCorners [[_,_,_],[_,_,_],[_,_,B]] = (True,8)
controlCorners _ = (False,0)

extraCondition :: Board -> (Bool, Int)
extraCondition x@[[_,_,_],[_,x1,x2],[x3,_,B]] = if x2==x3 && x1 /= x2 && x2 /= B then (True, 8) else controlCorners x
extraCondition _ = (False,0)

moves :: Board -> Player -> [Board]
moves board player
            | won board == 1 = []
            | won board == -1 = []
            | full board = []
            | otherwise = concat [move board index player | index <- [0..8]]

displayBoard :: Board -> String
displayBoard [] = []
displayBoard ([x1,x2,x3]:rest) = v1  ++ "|" ++  v2 ++ "|" ++ v3 ++ "\n" ++ displayBoard rest
                                where
                                    v1 = convertToString x1
                                    v2 = convertToString x2
                                    v3 = convertToString x3

displayBoard _ = ""

convertToString :: Player -> String
convertToString O = "O"
convertToString X = "X"
convertToString _ = " "
