> pairs :: [a] -> [[a]]
> pairs [] = []
> pairs (x:xs) = map (\a -> [x, a]) xs ++ pairs xs

> process :: (Eq a, Num a) => [[a]] -> a
> process = sum
>           . map product
>           . filter ((== 2020) . sum)

> partOne :: IO Int
> partOne = (process . pairs) <$> getInput

> triads :: [a] -> [[a]]
> triads [] = []
> triads (x:xs) = map ((:) x) (pairs xs) ++ triads xs

> partTwo :: IO Int
> partTwo = (process . triads) <$> getInput

> getInput :: IO [Int]
> getInput = (map read . lines) <$> readFile "01.txt"
