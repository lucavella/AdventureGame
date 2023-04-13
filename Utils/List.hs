module Utils.List (mapWithIndex, sublist, setAtIndex, sortF, sortF') where

    import Data.List (genericTake, genericDrop)
    

    mapWithIndex :: Integral n => (n -> a -> b) -> [a] -> [b]
    mapWithIndex = mapWithIndexIter 0

    mapWithIndexIter :: Integral n => n -> (n -> a -> b) -> [a] -> [b]
    mapWithIndexIter _ _ [] = []
    mapWithIndexIter i f (x : xs) =
        f i x : mapWithIndexIter (i + 1) f xs

    sublist :: Integral n => n -> n -> [a] -> [a]
    sublist _ _ [] = []
    sublist i len xs = genericTake len $ genericDrop i xs

    setAtIndex :: Integral n => n -> a -> [a] -> [a]
    setAtIndex i n xs = genericTake i xs ++ (n : genericDrop (i + 1) xs)

    -- Source: adapted quicksort implementation from lecture slides
    sortF :: Ord n => (a -> n) -> [a] -> [a]
    sortF _ [] = []
    sortF f (x : xs) =
        sortF f (filter ((>=) (f x) . f) xs) ++ [x] ++ 
        sortF f (filter ((<) (f x) . f) xs) --(\a -> f a > f x)
        
    sortF' :: Ord n => (a -> n) -> [a] -> [a]
    sortF' _ [] = []
    sortF' f (x : xs) =
        smaller `seq` larger `seq` smaller ++ [x] ++ larger
        where
            smaller = sortF' f (filter ((>=) (f x) . f) xs)
            larger = sortF' f (filter ((<) (f x) . f) xs)
