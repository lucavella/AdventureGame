{-# LANGUAGE RankNTypes #-}

module Utils.BFS (
    cheapestBFS, cheapestBFS'
) where


    -- function signature for expanding a state into possible next states
    type Expand a = a -> [a]
    -- function signature of a goal check for a state
    type GoalCheck a = a -> Bool


    -- breadth first search, takes an initial state, expand and goal check function
    -- returns the cheapest cost to reach a goal state
    -- keeps track of already visited states, so no same state is expanded multiple times
    cheapestBFS :: (Eq a, Ord n, Num n) => a -> Expand a -> GoalCheck a -> Maybe n
    cheapestBFS a exp gc
        | gc a = Just 0
        | otherwise = cheapestBFSIter [(a, 0)] [a] exp gc

    cheapestBFSIter :: (Eq a, Ord n, Num n) => [(a, n)] -> [a] -> Expand a -> GoalCheck a -> Maybe n
    cheapestBFSIter [] _ _ _ = Nothing
    cheapestBFSIter ((a, n) : as) seen exp gc =
        if any gc expA
        then Just $ n + 1
        else cheapestBFSIter as' seen' exp gc
        where
            expA = filter (\x -> not $ x `elem` seen) $ exp a
            seen' = seen ++ expA
            as' = as ++ map (\x -> (x, n + 1)) expA


    -- same as cheapestBFS' but without space leaks
    cheapestBFS' :: (Eq a, Ord n, Num n) => a -> Expand a -> GoalCheck a -> Maybe n
    cheapestBFS' a exp gc
        | gc a = Just 0
        | otherwise = cheapestBFSIter' [(a, 0)] [a] exp gc

    cheapestBFSIter' :: (Eq a, Ord n, Num n) => [(a, n)] -> [a] -> Expand a -> GoalCheck a -> Maybe n
    cheapestBFSIter' [] _ _ _ = Nothing
    cheapestBFSIter' ((a, n) : as) seen exp gc =
        if any gc expA
        then Just $ n + 1
        else seen' `seq` as' `seq` cheapestBFSIter' as' seen' exp gc
        where
            expA = filter (\x -> not $ x `elem` seen) $ exp a
            seen' = expA `seq` seen ++ expA
            as' = as ++ map (\x -> (x, n + 1)) expA
