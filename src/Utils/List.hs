module Utils.List (mapWithIndex, sublist, setAtIndex, sortF, sortF') where

    import Data.List (genericTake, genericDrop)
    

    -- same as map but the passed function also has access to the index of the element
    mapWithIndex :: Integral n => (n -> a -> b) -> [a] -> [b]
    mapWithIndex = mapWithIndexIter 0

    mapWithIndexIter :: Integral n => n -> (n -> a -> b) -> [a] -> [b]
    mapWithIndexIter _ _ [] = []
    mapWithIndexIter i f (x : xs) =
        f i x : mapWithIndexIter (i + 1) f xs

    -- keeps only the part of the list defined by the passed index range
    sublist :: Integral n => n -> n -> [a] -> [a]
    sublist _ _ [] = []
    sublist i len xs = genericTake len $ genericDrop i xs

    -- updates a list element on the given index
    setAtIndex :: Integral n => n -> a -> [a] -> [a]
    setAtIndex i n xs = genericTake i xs ++ (n : genericDrop (i + 1) xs)

    -- sorts a list based on the result of a function applied on each element
    -- source: adapted quicksort implementation from lecture slides
    sortF :: Ord n => (a -> n) -> [a] -> [a]
    sortF _ [] = []
    sortF f (x : xs) =
        sortF f (filter ((>=) (f x) . f) xs) ++ [x] ++ 
        sortF f (filter ((<) (f x) . f) xs) --(\a -> f a > f x)
    
    -- same as sortF but without space leaks
    sortF' :: Ord n => (a -> n) -> [a] -> [a]
    sortF' _ [] = []
    sortF' f (x : xs) =
        smaller `seq` larger `seq` smaller ++ [x] ++ larger
        where
            smaller = sortF' f (filter ((>=) (f x) . f) xs)
            larger = sortF' f (filter ((<) (f x) . f) xs)
