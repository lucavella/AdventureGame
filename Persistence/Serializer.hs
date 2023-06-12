{-# LANGUAGE RecordWildCards #-}

module Persistence.Serializer (saveGame) where

    import Models.Grid
    import Models.AdventureGame
    import Data.List
    import qualified Data.Set as S


    serializeParens :: String -> String
    serializeParens val = "( " ++ val ++ " )"

    serializeLine :: String -> String -> String
    serializeLine kw val = kw ++ " " ++ (serializeParens val)

    serializeLines :: String -> (a -> String) -> S.Set a -> [String]
    serializeLines kw shw = foldr (\a acc -> (serializeLine kw $ shw a) : acc) []

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
            serializeLine "position" (serializeCoordinate pos) ++ "\n" ++
            serializeLine "supply" (show water) ++ "\n" ++
            intercalate "\n" (concat [
                serializeLines "revealed" serializeCoordinate tilesVisited,
                serializeLines "collected" serializeCoordinate treasureCollected,
                serializeLines "emerging" serializeCoordinates wormsEmerging,
                serializeLines "disappearing" serializeCoordinates wormsDisappearing ])
            where
                Grid pos _ _ = grid


    saveGame :: String -> AdventureGameState -> IO ()
    saveGame fname = writeFile fname . show