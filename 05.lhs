> import Data.Bifunctor

> data Range a = Range {getMin :: a, getMax :: a}
>                deriving (Eq, Ord, Read, Show)

> halve :: Integral a => Bool -> Range a -> Range a
> halve upper (Range mn mx)
>     | upper     = Range (mp + 1) mx
>     | otherwise = Range mn mp
>     where mp = div (mn + mx) 2

> process :: Integral a => [Bool] -> Range a -> a
> process = fmap getMin . foldr (flip (.)) id . map halve

> getSeat :: String -> (Int, Int)
> getSeat = bimap
>           (flip process (Range 0 127) . map (== 'B'))
>           (flip process (Range 0 7) . map (== 'R'))
>           . span (flip elem "FB")

> seatID :: Num a => a -> a -> a
> seatID row column = 8 * row + column

> partOne :: String -> Int
> partOne = maximum . map (uncurry seatID . getSeat) . lines

> partTwo :: String -> [Int]
> partTwo s = rmMissing $ filter (not . flip elem seats) [0..(partOne s)]
>     where seats = map (uncurry seatID . getSeat) $ lines s

> rmMissing :: (Eq a, Num a) => [a] -> [a]
> rmMissing (b:bs) = rmMissing' b bs
>     where rmMissing' p (x:y:xs)
>               | y == x + 1 = rmMissing' y xs
>               | x /= p + 1 = x : rmMissing' x (y:xs)
>               | otherwise = rmMissing' x (y:xs)
>           rmMissing' p (x:[]) = if x == p + 1 then [] else [x]
>           rmMissing' _ xs = xs
