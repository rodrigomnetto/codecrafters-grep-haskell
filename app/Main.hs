module Main where

import System.Environment
import System.Exit
import Data.Char
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))

matchPattern :: String -> String -> Bool
matchPattern pattern input =
  if length pattern == 1
    then head pattern `elem` input
    else
      let (f, _) = parsePattern pattern
      in f (head input)

parsePattern :: String -> (Char -> Bool, String)
parsePattern pattern =
  case pattern of
    '\\' : r1 -> case r1 of
      'd' : r2 -> (isDigit, r2)
      'w' : r2 -> (isDigitOrLetter, r2)
    '[' : r1  -> case r1 of
      '^' : r2 -> 
        let (f1, r3) = isAny r2
        in (not . f1, r3)
      _        ->
        isAny r1
      
isChar :: Char -> Char -> Bool
isChar chr c = chr == c

isAny :: String -> (Char -> Bool, String)
isAny []         = (\_ -> False, [])
isAny (']' : xs) = (\_ -> False, xs)
isAny (x : xs)   =
  let (f1, r) = isAny xs
  in (\y -> isChar x y || f1 y, r)

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

  -- Check for -E flag
  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else
      if matchPattern pattern input_line
        then exitSuccess
        else exitFailure
