module CamelCase (toCamelCase) where

import Data.Char (toUpper)
import Data.List.Split (splitOneOf)

toCamelCase :: String -> String
toCamelCase str =
  let tokens :: [String]
      tokens = splitOneOf "-_" str
      capFirst :: String -> String
      capFirst [] = []
      capFirst (h : t) = (toUpper h) : t
  in case tokens of
       [] -> []
       (h : t) -> foldl (\acc token -> acc ++ (capFirst token)) h t
