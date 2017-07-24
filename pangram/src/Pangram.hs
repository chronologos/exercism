module Pangram (isPangram) where
import Data.Set
import Data.Char (toLower, isLetter)

isPangram :: String -> Bool
isPangram text = getCharSet text == Data.Set.fromList "abcdefghijklmnopqrstuvwxyz"

getCharSet :: String -> Set Char
getCharSet = Prelude.foldl
               (\acc x -> if
                            (not . isLetter) x
                          then
                            acc
                          else
                            insert (toLower x) acc
               ) empty
