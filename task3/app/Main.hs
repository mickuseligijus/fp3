module Main where

import Lib
import System.IO (hPutStrLn, stderr)
import System.Exit
import System.Environment

main :: IO ()
main = do
    message <- getLine
    player <- getArgs
    (codedMessage,erMessage,code) <- mutation message player
    putStrLn codedMessage
    hPutStrLn stderr erMessage
    case code of
        0 -> exitSuccess
        a -> exitWith (ExitFailure code)
