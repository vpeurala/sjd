module Util where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)

indent :: String -> String
indent source = unlines $ map (\line -> if isBlank line then "" else "    " ++ line) (lines source)

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
  else x ++ "\n" ++ separateNonBlanksWithNewline xs
