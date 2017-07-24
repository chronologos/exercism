module RunLength (decode, encode) where
import Data.Char (intToDigit, isDigit, digitToInt)
import Data.List (group)
import Control.Arrow

encode :: String -> String
encode xs = foldl
              encodeHelper
              ""
              (map (length &&& head ) (group xs))

encodeHelper :: String -> (Int, Char) -> String
encodeHelper x (1,z) = x ++ [z]
encodeHelper x (y,z) = x ++ (show y ++ [z])

decode :: String -> String
decode = decodeHelper ""
-- A2BC -> ABBC
decodeHelper :: String -> String -> String
-- takes string to build, string to decode, and returns decoded string.
decodeHelper b "" = b
decodeHelper b a
          | isDigit $ head a = decodeHelper (b++(replicate
                                        (read $ takeWhile isDigit a :: Int)
                                        (head $ dropWhile isDigit a)
                                    )) (drop 1 $ dropWhile isDigit a)
          | otherwise = decodeHelper (b ++ [head a]) (tail a)
