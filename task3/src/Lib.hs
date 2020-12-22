module Lib
where

import Convert
import Tictactoe
import Data.List
import Data.Function


mutation :: String -> [String] ->IO (String,String,Int)
mutation ('*':_) (p:_) = return (messageToPrint, messageEr, 0)
                    where 
                        messageEr = displayBoard [[X,B,B],[B,B,B],[B,B,B]] ++ " My move is "++ "(0,0," ++ p ++ ")"
                        messageToPrint = convertToDicFirst p

mutation message (p:_) =
    case convertOrder message of
        Left MessageMalformed -> return ("","", 100)
        Left Order -> return ("","", 101)
        Left Duplicates -> return ("","", 100)
        Right value -> 
                if won finalBoard == 1 || won finalBoard == -1 || full finalBoard then
                    return ("","",20)
                else
                     return (messageToPrint, messageEr, code)
                        where
                            replacedBoard = replaceInitialBoard value
                            sortedBoard = sortBoard replacedBoard
                            finalBoard = changeForm sortedBoard
                            newBoard = pickMove finalBoard (convertPlayer p)
                            myMove = getMove newBoard finalBoard 0
                            messageEr = displayBoard newBoard ++ " My move is " ++ show myMove
                            messageToPrint = convertToDic message myMove
                            code = getCode newBoard

convertPlayer :: String -> Player
convertPlayer "X" = X
convertPlayer "O" = O

                        -- "O" -> return (messageToPrint, messageEr, code)
                        --     where 
                        --         newBoard = pickMove finalBoard O
                        --         myMove = getMove newBoard finalBoard 0
                        --         messageEr = displayBoard finalBoard ++ " My move is " ++ show myMove-- finalBoard turetu but
                        --         messageToPrint = convertToDic message myMove
                        --         code = getCode newBoard
                                
getCode :: Board -> Int
getCode board
        | won board == 1 = 10
        | full board = 12
        | otherwise = 0

getMove :: Board -> Board -> Int -> (Int,Int,Player)
getMove (x:rest) (x1:rest2) counter = if x == x1 then getMove rest rest2 (counter+1) -- first argument newBoard
                                else getMove' x x1 0 counter

getMove' :: [Player] -> [Player] -> Int -> Int -> (Int,Int,Player)
getMove' (a:rest) (b:rest2) xCounter yCounter = if a == b then getMove' rest rest2 (xCounter+1) yCounter
                                                else (xCounter,yCounter,a)

convertToDicFirst :: String ->  String
convertToDicFirst p1 = "d4:lastd1:0d4:datad1:0i0e1:1i0e1:21:" ++ p1 ++ "eeee"

convertToDic :: String -> (Int,Int,Player) -> String
convertToDic message (x,y,v) = "d4:prev" ++ message ++ "4:lastd1:0d4:datad1:0i" ++ show x ++ "e1:1i" ++ show y ++ "e1:21:"++ show v ++"eeee"


