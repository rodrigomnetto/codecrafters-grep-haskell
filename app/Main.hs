{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant lambda" #-}

module Main where

import System.Environment
import System.Exit
import Data.Char
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import GHC.Generics (Meta)


type Metadata = (Int, [(Int, Char)])
type Regex = (Metadata -> (Bool, Metadata)) --tipo mais complexo, fn, parsed, groups

matchPattern :: Metadata -> Regex -> Bool
matchPattern metadata regex
  | isEmpty metadata = False -- nao pode retornar falso sempre, vazio é vazio, pode dar match
  | otherwise =
    let (r, rest) = regex metadata in
    if r
      then r
      else matchPattern rest regex

parseRegex :: String -> [Regex] -> Regex
parseRegex [] l = loopRegex (reverse l)
parseRegex pattern l =
  let (rest, list) = case pattern of
        '(' : r1 -> 
              let (grp, r2) = captureGroup r1 
                  grpRegex = parseRegex grp []
              in
                build grpRegex l r2
        '|' : r1 -> build (orElse (loopRegex (reverse l)) (parseRegex r1 [])) [] []
        '^' : r1 -> build startAnchor l r1
        '$' : r1 -> build endAnchor l r1
        '.' : r1 -> build anyCharacter l r1
        '\\': r1 -> case r1 of
          'd' : r2 -> build isDigit2 l r2
          'w' : r2 -> build isDigitOrLetter l r2
        '[' : r1 -> case r1 of
          '^' : r2 -> let (rgx, rest1) = isNotAny r2 in build rgx l rest1
          _        -> let (rgx, rest1) = isAny r1 in build rgx l rest1
        '+' : r1 -> build (oneOrMore (head l) r1) (tail l) r1
        '?' : r1 -> build (zeroOrOne (head l)) (tail l) r1
        chr : r1 -> build (isChar chr) l r1
  in
    parseRegex rest list

captureGroup :: String -> (String, String)
captureGroup (')':xs) = ([], xs)
captureGroup (x:xs) =
  let (grp, rest) = captureGroup xs in
    (x:grp, rest)

loopRegex :: [Regex] -> Metadata -> (Bool, Metadata)
loopRegex [] metadata = (True, metadata)
loopRegex (x:xs) metadata =
    let (r1, rest) = x metadata in
      if r1
        then loopRegex xs rest
        else (False, rest)

build :: Regex -> [Regex] -> String -> (String, [Regex])
build regex list rest =
  (rest, regex:list)

orElse :: Regex -> Regex -> Regex
orElse f1 f2 =
  \x -> let (r1, rest1) = f1 x in
    if r1
      then (r1, rest1)
      else f2 x

anyCharacter :: Regex
anyCharacter metadata
  | isEmpty metadata = (False, metadata)
  | otherwise = (True, getTail metadata)

oneOrMore :: Regex -> String -> Regex
oneOrMore regex rgxIn metadata
  | isEmpty metadata = (False, metadata)
  | otherwise =
    let (r, rest) = regex metadata in
      if r
        then
            let parsedRgx = parseRegex rgxIn []
                (r2, _) = parsedRgx rest
                (r1, rest1) = oneOrMore regex rgxIn rest in
            if r2 || not r1
              then (r, rest)
              else (r1, rest1)
        else
          (r, rest)

zeroOrOne :: Regex -> Regex
zeroOrOne regex metadata =
  let (r, rest) = regex metadata in
    if r
      then (r, rest)
      else (True, metadata)

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

getList :: Metadata -> [(Int, Char)]
getList (_, l) = l

clean :: Metadata -> Metadata
clean (s, _) = (s, [])

--end of metadata functions

--fixes (zzz|jjj)?$
--loop infinito quando input é jjjj
--da erro quando input é vazio
--aprender a debugar em haskell
--deve consumir o caractere quando um erro ocorre?


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
      let regex = parseRegex pattern []
          metadata = extractMetadata input_line
      in
        if getSize metadata == 0
          then 
            let (r, _) = regex metadata in
              if r then exitSuccess else exitFailure
          else
            if matchPattern metadata regex
              then exitSuccess
              else exitFailure
