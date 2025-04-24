{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant lambda" #-}

module Main where

import System.Environment
import System.Exit
import Data.Char
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import GHC.Generics (Meta)


type Metadata = (Int, [(Int, Char)]);
type Regex = (Metadata -> (Bool, Metadata))

matchPattern :: Metadata -> Regex -> Bool
matchPattern metadata regex
  | isEmpty metadata = False
  | otherwise = 
    let (r, rest) = regex metadata in
    if r
      then r
      else matchPattern rest regex

parseRegex :: String -> Regex
parseRegex [] = \x -> (True, x)
parseRegex pattern =
  let (f, rest) = case pattern of
        '^' : r1 -> (startAnchor, r1)
        '$' : r1 -> (endAnchor, r1)
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

startAnchor :: Regex
startAnchor metadata = 
  let isBegin = getIndex metadata == 0 in
    if isBegin
    then (True, metadata)
    else (False, clean metadata)

endAnchor :: Regex
endAnchor metadata = (isEmpty metadata, metadata)

isDigit2 :: Regex
isDigit2 metadata 
  | isEmpty metadata = (False, metadata)
  | otherwise =
    let chr = getCharacter metadata
        tailMetadata = getTail metadata
    in
      (isDigit chr, tailMetadata)

isChar :: Char -> Regex
isChar char metadata
  | isEmpty metadata = (False, metadata)
  | otherwise = 
    let chr = getCharacter metadata
        tailMetadata = getTail metadata
    in
      (char == chr, tailMetadata)

--improve
isNotAny :: String -> (Regex, String)
isNotAny pattern =
  let (rgx, ptr) = isAny pattern
  in (\input ->
        let (r, rest) = rgx input
        in (not r, rest), ptr)

isAny :: String -> (Regex, String)
isAny (']' : xs) = (\x -> (False, getTail x), xs)
isAny (x:xs) =
  let (p, r) = isAny xs
  in (isChar x `orElse` p , r)

isDigitOrLetter :: Regex
isDigitOrLetter metadata 
  | isEmpty metadata = (False, metadata)
  | otherwise = 
    let chr = getCharacter metadata
        tailMetadata = getTail metadata
    in
      (isDigit chr || isAlpha chr, tailMetadata)

--metadata functions

extractMetadata :: String -> Metadata
extractMetadata input = 
  let size = length input 
  in
    (size, zip [0..size - 1]  input) 

isEmpty :: Metadata -> Bool
isEmpty (_, []) = True
isEmpty _ = False

getCharacter :: Metadata -> Char
getCharacter (_, (_,c):_) = c

getIndex :: Metadata -> Int
getIndex (_, (i,_):_) = i

getSize :: Metadata -> Int --maybe its not necessary
getSize (s, _) = s

getTail :: Metadata -> Metadata
getTail (s, x:xs) = (s, xs)

clean :: Metadata -> Metadata
clean (s, _) = (s, [])

--end of metadata functions

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
      let regex = parseRegex pattern
          metadata = extractMetadata input_line 
      in
        if matchPattern metadata regex
          then exitSuccess
          else exitFailure
