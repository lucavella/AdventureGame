module Utils.List (
    sublist, reverseAppend, reverseSplitAt, reverseMap, mapWithIndex, setAtIndex, sortF, sortF'
) where

    import Data.List (genericTake, genericDrop)
    

    -- keeps only the part of the list defined by the passed index range
    sublist :: Integral n => n -> n -> [a] -> [a]
    sublist _ _ [] = []
    sublist i j xs = genericTake (j - i + 1) $ genericDrop i xs

    -- reverses the first list and appends the second to it
    -- without iterating twice over the list (ie. reverse and append) 
    reverseAppend :: [a] -> [a] -> [a]
    reverseAppend [] ys = ys
    reverseAppend (x : xs) ys = reverseAppend xs (x : ys)

    -- same as splitAt, but generic and with front part of list revresed
    -- without iterating twice over list (ie. splitAt and reverse)
    reverseSplitAt :: Integral n => n -> [a] -> ([a], [a])
    reverseSplitAt = reverseSplitAtIter []

    reverseSplitAtIter :: Integral n => [a] -> n -> [a] -> ([a], [a])
    reverseSplitAtIter acc 0 xs = (acc, xs)
    reverseSplitAtIter acc n (x : xs) = reverseSplitAtIter (x : acc) (n - 1) xs

    -- reverses list and maps function over it
    -- without iterating twice over list (ie. map and reverse)
    reverseMap :: (a -> b) -> [a] -> [b]
    reverseMap = reverseMapIter []

    reverseMapIter :: [b] -> (a -> b) -> [a] -> [b]
    reverseMapIter acc _ [] = acc
    reverseMapIter acc f (x : xs) =
        reverseMapIter (f x : acc) f xs


    -- same as map but the passed function also has access to the index of the element
    mapWithIndex :: Integral n => (n -> a -> b) -> [a] -> [b]
    mapWithIndex = mapWithIndexIter 0

    mapWithIndexIter :: Integral n => n -> (n -> a -> b) -> [a] -> [b]
    mapWithIndexIter _ _ [] = []
    mapWithIndexIter i f (x : xs) =
        f i x : mapWithIndexIter (i + 1) f xs

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
