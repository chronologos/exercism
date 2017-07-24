module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA xs = foldl helper (Just "") (map transcribeOne xs)

helper :: Maybe String -> Maybe Char -> Maybe String
helper Nothing _ = Nothing
helper (Just _) Nothing = Nothing
helper (Just x) (Just y) = Just (x++[y])

transcribeOne :: Char -> Maybe Char
transcribeOne 'A' = Just 'U'
transcribeOne 'G' = Just 'C'
transcribeOne 'C' = Just 'G'
transcribeOne 'T' = Just 'A'
transcribeOne _   = Nothing
