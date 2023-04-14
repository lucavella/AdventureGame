{-# LANGUAGE RankNTypes #-}

module Utils.UniformCostSearch (cheapestUC, cheapestUC') where

    import Utils.List (sortF, sortF')

    -- function signature for expanding a state into possible next states
    type Expand a = a -> [a]
    -- function signature of a goal check for a state
    type GoalCheck a = a -> Bool


    -- uniform cost search implementation that takes an initial state, expand and goal check function
    -- returns the cheapest cost to reach a goal state
    -- keeps track of already visited states, so no same state is expanded multiple times
    cheapestUC :: (Eq a, Ord n, Num n) => a -> Expand a -> GoalCheck a -> Maybe n
    cheapestUC a =
        cheapestUCIter [(a, 0)] [a]

    cheapestUCIter :: (Eq a, Ord n, Num n) => [(a, n)] -> [a] -> Expand a -> GoalCheck a -> Maybe n
    cheapestUCIter [] _ _ _ = Nothing
    cheapestUCIter ((a, n) : as) seen exp gc
        | gc a      = Just $ n
        | otherwise =
            cheapestUCIter as' seen' exp gc
            where
                expA = filter (\x -> not $ x `elem` seen) $ exp a
                seen' = seen ++ expA
                as' = sortF snd $ map (\x -> (x, n + 1)) expA ++ as


    -- same as cheapestUC' but without space leaks
    cheapestUC' :: (Eq a, Ord n, Num n) => a -> Expand a -> GoalCheck a -> Maybe n
    cheapestUC' a =
        cheapestUCIter' [(a, 0)] [a]

    cheapestUCIter' :: (Eq a, Ord n, Num n) => [(a, n)] -> [a] -> Expand a -> GoalCheck a -> Maybe n
    cheapestUCIter' [] _ _ _ = Nothing
    cheapestUCIter' ((a, n) : as) seen exp gc
        | gc a      = Just $ n
        | otherwise =
            as' `seq` cheapestUCIter' as' seen' exp gc
            where
                expA = filter (\x -> not $ x `elem` seen) $ exp a
                seen' = expA `seq` seen ++ expA
                as' = sortF' snd $ map (\x -> (x, n + 1)) expA ++ as
