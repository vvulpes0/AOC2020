> import Data.Set (Set)
> import qualified Data.Set as Set

> import Common

> partOne :: String -> Int
> partOne = sum . map (Set.size . Set.fromList . concat)
>           . splitOn null . lines

> intersections :: Ord a => [Set a] -> Set a
> intersections (x:xs) = foldr Set.intersection x xs
> intersections _ = Set.empty

> partTwo :: String -> Int
> partTwo = sum . map (Set.size . intersections . map Set.fromList)
>           . splitOn null . lines
