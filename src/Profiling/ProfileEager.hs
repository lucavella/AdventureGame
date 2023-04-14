{-# LANGUAGE RecordWildCards #-}

import Models.TerminalGame
import Models.AdventureGame
import Models.Grid
import Utils.UniformCostSearch
import Data.Maybe (catMaybes)


eagerClosestPortal' :: (Num n, Ord n) => Grid -> Maybe n
eagerClosestPortal' grid =
    cheapestUC' (0, 0) expand goalCheck
    where
        expand coord = 
            filter (\c -> elem (tileType $ getTile grid c) allowedTiles) . catMaybes $ map (makeMove coord) [(UpMove)..(LeftMove)]
        goalCheck coord =
            (tileType $ getTile grid coord) == Portal
        allowedTiles = [Water, Desert False, Desert True, Portal]
        
main :: IO ()
main = print eagerPortalDistance
    where
        grid = initGrid $ AdventureGameConfig {
            seed = 56133,
            sight = 5,
            waterCap = 120,
            treasurePct = 50,
            waterPct = 1,
            portalPct = 0.01,
            lavaSinglePct = 40,
            lavaAdjacentPct = 50,
            gridDim = (30, 20)
        }
        eagerPortalDistance = eagerClosestPortal' grid
