module Main where

import System.Environment
import System.Exit
import Data.Char
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))

matchPattern :: String -> String -> Bool
matchPattern pattern input = do
  if length pattern == 1
    then head pattern `elem` input
    else 
      if length pattern == 2
        then case pattern of
          "\\d" -> isDigit $ head input
          "\\w" -> isDigitOrLetter $ head input
          _     -> error $ "Unhandled pattern: " ++ pattern
        else error $ "Unhandled pattern: " ++ pattern


isDigitOrLetter :: Char -> Bool
isDigitOrLetter c = isDigit c || isAlpha c

main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  args <- getArgs
  let pattern = args !! 1
  input_line <- getLine

  -- Uncomment this block to pass stage 1
  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else do if matchPattern pattern input_line
              then exitSuccess
              else exitFailure