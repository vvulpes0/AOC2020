> import Data.Bifunctor

> parseLine :: String -> (Int, Int, Char, String)
> parseLine s = let (n, r1) = bimap read (drop 1) $ break (== '-') s
>                   (x, r2) = bimap read (drop 1) $ break (== ' ') r1
>                   (c, p) = fmap (drop 2) $ splitAt 1 r2
>               in (n, x, head c, p)

> isValid1 :: (Int, Int, Char, String) -> Bool
> isValid1 (n, x, c, p) = inRange n x . length $ filter (== c) p

> inRange :: (Ord a) => a -> a -> a -> Bool
> inRange n x a = n <= a && a <= x

> partOne :: IO Int
> partOne = length <$> filter (isValid1 . parseLine) <$> lines
>           <$> readFile "02.txt"

> isValid2 :: (Int, Int, Char, String) -> Bool
> isValid2 (n, x, c, p) = ((p !! (n - 1)) == c) /= ((p !! (x - 1)) == c)

> partTwo :: IO Int
> partTwo = length <$> filter (isValid2 . parseLine) <$> lines
>           <$> readFile "02.txt"
