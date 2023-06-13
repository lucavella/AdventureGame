{-# LANGUAGE RecordWildCards #-}

module Persistence.Serializer (saveGame) where

    import Models.Grid
    import Models.AdventureGame
    import Data.List


    -- serializes enclosed in parenthesis
    serializeParens :: String -> String
    serializeParens val = "( " ++ val ++ " )"

    -- serializes a line for the save file
    serializeLine :: String -> String -> String
    serializeLine kw val = kw ++ " " ++ (serializeParens val)

    -- serializesall foldable data structure (set or list) for the save file
    serializeLineFold :: Foldable t => String -> (a -> String) -> t a -> [String]
    serializeLineFold kw shw = foldr (\a acc -> (serializeLine kw $ shw a) : acc) []

    -- serializes a Coordinate (it is a custom show in essense)
    -- cannot make Coordinate instance of show as it is a type synonym of a tuple
    -- making Coordinate a newtype requires refractoring a lot of code
    serializeCoordinate :: Coordinate -> String
    serializeCoordinate (x, y) =
        "[ " ++ show x ++ " , " ++ show y ++ " ]"

    -- serializes a list of coordinates
    serializeCoordinates :: [Coordinate] -> String
    serializeCoordinates cs = intercalate " , " $ map serializeCoordinate cs


    -- serialize game config for save file
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

    -- serializes game state (including game config) for save file
    instance Show AdventureGameState where
        show AdventureGameState{..} =
            show gameConfig ++ "\n" ++
            serializeLine "position" (serializeCoordinate pos) ++ "\n" ++
            serializeLine "supply" (show water) ++ "\n" ++
            intercalate "\n" (concat [
                serializeLineFold "revealed" serializeCoordinate tilesVisited,
                serializeLineFold "collected" serializeCoordinate treasureCollected,
                serializeLineFold "emerging" serializeCoordinates wormsEmerging,
                serializeLineFold "disappearing" serializeCoordinates wormsDisappearing ])
            where
                Grid pos _ _ = grid


    -- saves the game state
    saveGame :: String -> AdventureGameState -> IO ()
    saveGame fname = writeFile fname . show