module Util where

import Data.Char (toLower, toUpper)

indent :: String -> String
indent source = unlines $ fmap (\line -> if isBlank line then "" else mappend "    " line) (lines source)

isBlank :: String -> Bool
isBlank = all (`elem` "\t ")

upcase :: String -> String
upcase [] = []
upcase (x:xs) = toUpper x : xs

downcase :: String -> String
downcase [] = []
downcase (x:xs) = toLower x : xs

separateNonBlanksWithNewline :: [String] -> String
separateNonBlanksWithNewline []     = ""
separateNonBlanksWithNewline [x]    = x
separateNonBlanksWithNewline (x:xs) =
  if isBlank x
  then separateNonBlanksWithNewline xs
  else mconcat [x, "\n", separateNonBlanksWithNewline xs]
