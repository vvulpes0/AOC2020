> import Data.Char (isDigit, isSpace)
> import Data.List (intersperse)
> import Data.Set (Set)
> import qualified Data.Set as Set

> splitOn :: (a -> Bool) -> [a] -> [[a]]
> splitOn f xs
>     | null xs = []
>     | otherwise = uncurry (:) . fmap (splitOn f)
>                   . break f $ dropWhile f xs

> parse :: String -> [(String, String)]
> parse = map (f . splitOn (== ':'))
>         . splitOn isSpace
>     where f [] = ("", "")
>           f (x:xs) = (x, concat $ intersperse ":" xs)

> isValid :: [(String, String)] -> Bool
> isValid = Set.isSubsetOf keys . Set.fromList . map fst
>     where keys = Set.fromList
>                  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

> separate :: String -> [String]
> separate = map (concat . intersperse " ") . splitOn null . lines

> partOne :: String -> Int
> partOne = length . filter (isValid . parse) . separate

> isStrict :: [(String, String)] -> Bool
> isStrict [] = True
> isStrict ((k,v):xs)
>     | k == "byr" = f (range 1920 2002 v)
>     | k == "iyr" = f (range 2010 2020 v)
>     | k == "eyr" = f (range 2020 2030 v)
>     | k == "hgt" = let (v1, r) = span isDigit v
>                    in case r of
>                         "cm" -> f (range 150 193 v1)
>                         "in" -> f (range 59 76 v1)
>                         _ -> False
>     | k == "hcl" = case v of
>                      '#' : v1 -> f (hcl v1)
>                      _ -> False
>     | k == "ecl" = f (elem v ["amb","blu","brn","gry","grn","hzl","oth"])
>     | k == "pid" = f (all isDigit v && length v == 9)
>     | k == "cid" = f True
>     where f p = p && isStrict xs
>           range n x s = all isDigit s && read s >= n && read s <= x
>           hcl s = length s == 6 && all (flip elem "0123456789abcdef") s

> both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
> both f g x = f x && g x

> partTwo :: String -> Int
> partTwo = length . filter (both isValid isStrict . parse) . separate
