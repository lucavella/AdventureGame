{-# LANGUAGE RecordWildCards #-}

module Persistence.Serializer (gameStateSerializer) where

    import Models.Grid
    import Models.AdventureGame


    serializeParens :: String -> String
    serializeParens val = "( " ++ val ++ " )"

    serializeLine :: String -> String -> String
    serializeLine kw val = kw ++ " " ++ (serializeParens $ show val)

    serializeLines :: String -> (a -> String) -> [a] -> String
    serializeLines kw shw = unlines . map (serializeLine kw . shw)

    -- custom show for Coordinate
    -- cannot make Coordinate instance of show as it is a type synonym of a tuple
    -- making Coordinate a newtype requires refractoring a lot of code
    serializeCoordinate :: Coordinate -> String
    serializeCoordinate (x, y) =
        "[ " ++ show x ++ " , " ++ show y ++ " ]"

    serializeCoordinates :: [Coordinate] -> String
    serializeCoordinates [] = ""
    serializeCoordinates (c : cs) =
        serializeCoordinate c ++ " , " ++ serializeCoordinates cs


    instance Show AdventureGameConfig where
        show AdventureGameConfig{..} =
            serializeLine "s" (show sight) ++ "\n" ++
            serializeLine "m" (show waterCap) ++ "\n" ++
            serializeLine "g" (show seed) ++ "\n" ++
            serializeLine "t" (show treasurePct) ++ "\n" ++
            serializeLine "w" (show waterPct) ++ "\n" ++
            serializeLine "p" (show portalPct) ++ "\n" ++
            serializeLine "l" (show lavaSinglePct) ++ "\n" ++
            serializeLine "ll" (show lavaAdjacentPct) ++ "\n" ++
            serializeLine "x" (show wormLength) ++ "\n" ++
            serializeLine "y" (show wormSpawnPct)

    instance Show AdventureGameState where
        show AdventureGameState{..} =
            show gameConfig ++ "\n" ++
            serializeLine "position" (show pos) ++ "\n" ++
            serializeLine "water" (show water) ++ "\n" ++
            serializeLines "revealed" serializeCoordinate tilesVisited ++ "\n" ++
            serializeLines "collected" serializeCoordinate treasureCollected ++ "\n" ++
            serializeLines "emerging" serializeCoordinates wormsEmerging ++ "\n" ++
            serializeLines "disappearing" serializeCoordinates wormsDisappearing
            where
                Grid pos _ _ = grid


    gameStateSerializer :: AdventureGameState -> String
    gameStateSerializer = show