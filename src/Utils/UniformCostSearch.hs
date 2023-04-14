{-# LANGUAGE RankNTypes #-}

module Utils.UniformCostSearch (cheapestUC, cheapestUC') where

    import Utils.List (sortF, sortF')


    type Expand a = a -> [a]
    type GoalCheck a = a -> Bool

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
