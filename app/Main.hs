{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant lambda" #-}

module Main where

import System.Environment
import System.Exit
import Data.Char
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))


type Regex = (String -> (Bool, String))

matchPattern :: String -> Regex -> Bool
matchPattern [] _ = False
matchPattern input regex =
  let (r, rest) = regex input in
    if r
      then r
      else matchPattern rest regex

parseRegex :: String -> Regex
parseRegex [] = \x -> (True, x)
parseRegex pattern =
  let (f, rest) = case pattern of
        '\\': r1 -> case r1 of
          'd' : r2 -> (isDigit2, r2)
          'w' : r2 -> (isDigitOrLetter, r2)
        '[' : r1  -> case r1 of
          '^' : r2 -> isNotAny r2
          _        -> isAny r1
        x   : xs -> (isChar x, xs)
  in
    f `andThen` parseRegex rest

andThen :: Regex -> Regex -> Regex
andThen f1 f2 =
  \x -> let (r1, rest1) = f1 x in
    if r1
      then f2 rest1
      else (r1, rest1)

orElse :: Regex -> Regex -> Regex
orElse f1 f2 =
  \x -> let (r1, rest1) = f1 x in
    if r1
      then (r1, rest1)
      else f2 x

isDigit2 :: Regex
isDigit2 [] = (False, [])
isDigit2 (x:xs) = (isDigit x, xs)

isChar :: Char -> Regex
isChar chr [] = (False, [])
isChar chr (x:xs) = (x == chr, xs)

isNotAny :: String -> (Regex, String)
isNotAny pattern =
  let (rgx, ptr) = isAny pattern
  in (\input ->
        let (r, rest) = rgx input
        in (not r, rest), ptr)

isAny :: String -> (Regex, String)
isAny (']' : xs) = (\x -> (False, x), xs)
isAny (x:xs) =
  let (p, r) = isAny xs
  in (isChar x `orElse` p , r)

isDigitOrLetter :: Regex
isDigitOrLetter [] = (False, [])
isDigitOrLetter (x:xs) = (isDigit x || isAlpha x, xs)

main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  args <- getArgs
  let pattern = args !! 1
  input_line <- getLine

  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else
      let regex = parseRegex pattern in
        if matchPattern input_line regex
          then exitSuccess
          else exitFailure
