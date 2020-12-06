> geo = ["..##......."
>       ,"#...#...#.."
>       ,".#....#..#."
>       ,"..#.#...#.#"
>       ,".#...##..#."
>       ,"..#.##....."
>       ,".#.#.#....#"
>       ,".#........#"
>       ,"#.##...#..."
>       ,"#...##....#"
>       ,".#..#...#.#"]

> slope :: Int -> Int -> [String] -> Int
> slope right down xs
>     = case xs of
>         [] -> 0
>         (x:_) -> case x of
>                    [] -> 0
>                    (a:_) -> (if a == '#' then 1 else 0)
>                             + slope right down xs'
>       where xs' = map (drop right) $ drop down xs

> partOne :: IO Int
> partOne = slope 3 1 <$> map cycle <$> lines <$> readFile "03.txt"

> partTwo' :: [String] -> Int
> partTwo' = product
>            . flip map [(1,1),(3,1),(5,1),(7,1),(1,2)]
>            . (\c -> (\(a, b) -> slope a b c))
>            . map cycle

> partTwo = partTwo' <$> lines <$> readFile "03.txt"
