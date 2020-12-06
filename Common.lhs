> module Common where

> splitOn :: (a -> Bool) -> [a] -> [[a]]
> splitOn f xs
>     | null xs = []
>     | otherwise = uncurry (:) . fmap (splitOn f)
>                   . break f $ dropWhile f xs
