module Bob (responseFor) where
import Data.Char (toUpper)

responseFor :: String -> String
responseFor [] = "Fine. Be that way!"
responseFor xx
              | isProlongedSilence x = "Fine. Be that way!"
              | isSymbols x && isForceful x && isQuestion x = "Sure."
              | isQuestion x && isForceful x = "Whoa, chill out!"
              | isQuestion x && isSymbols x = "Sure."
              | isSymbols x && isForceful x && last x /= '!' = "Whatever."
              | isSymbols x && isForceful x = "Whoa, chill out!"
              | isQuestion x = "Sure."
              | isForceful x = "Whoa, chill out!"
              | isSymbols x = "Whatever."
              where x = trim xx

responseFor _ = "Whatever."

trim :: String -> String
trim = foldr (\y acc -> if y `elem` " \t\n\r" then acc else y:acc) ""

isProlongedSilence :: String -> Bool
isProlongedSilence = foldl (\acc y -> if y `elem` " \t" then acc && True else acc && False) True

isQuestion :: String -> Bool
isQuestion x = last x == '?'

isForceful :: String -> Bool
isForceful x = map toUpper x == x

isSymbols :: String -> Bool
isSymbols = foldl (\acc y -> if y `elem` "0123456789 ,;:[{}]()?!@#$%^&*" then acc && True else acc && False) True
