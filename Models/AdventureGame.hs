{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.AdventureGame (
    AdventureGameConfig(..), AdventureGameState(..), AdventureGameEvent(..),
    isValidGameConfig, closestDistance, initGrid
) where

    import Models.Grid
    import Models.GameState
    import Utils.List (mapWithIndex, sublist)
    import Utils.BFS (cheapestBFS')
    import Data.Char (toLower)
    import Data.Maybe (catMaybes)
    import Data.List (intercalate)
    import qualified Data.Set as S
    import System.Random (StdGen, RandomGen, mkStdGen, random, randomR)
    import Text.Read (ReadS, readMaybe)


    -- game configuration data type that defines a unique initial game state
    -- the required parameters are commented by the letter as they appear in the assignment 
    data AdventureGameConfig = AdventureGameConfig {
        seed :: Int, -- g
        sight :: Int, -- s
        waterCap :: Int, -- m
        treasurePct :: Double, -- t
        waterPct :: Double, -- w
        portalPct :: Double, -- p
        lavaSinglePct :: Double, -- l
        lavaAdjacentPct :: Double, -- ll
        wormLength :: Int, -- x
        wormSpawnPct :: Double -- y
    }

    -- game state data type that contains all information at a given point in the game
    data AdventureGameState = AdventureGameState {
        grid :: Grid,
        event :: AdventureGameEvent,
        water :: Int,
        tilesVisited :: S.Set Coordinate,
        treasureCollected :: S.Set Coordinate,
        wormsEmerging :: S.Set [Coordinate],
        wormsDisappearing :: S.Set [Coordinate],
        gameConfig :: AdventureGameConfig,
        message :: Maybe String
    }

    -- data type used in the game's state to keep track of what happened
    data AdventureGameEvent = NoEvent | CollectedTreasure | ReplenishedWater | DiedOfThirst | DiedOfLava | DiedOfWorm | Won
        deriving Eq

    instance Show AdventureGameEvent where
        show event = case event of
            NoEvent -> "You roam an empty and barren land."
            CollectedTreasure -> "You found a treasure!"
            ReplenishedWater -> "You refilled your water bottle."
            DiedOfThirst -> "You died of thirst."
            DiedOfLava -> "You died from lava."
            DiedOfWorm -> "A mighty sandworm has devoured you."
            Won -> "You safely escaped with all collected treasure!"
    
    -- GameState and TerminalGame instance so AdventureGameState can be used in runGame
    instance GameState AdventureGameState PlayerMove where
        nextState state@AdventureGameState{..} cmd =
            pure . (makeStateChange state) =<< makeMoveGrid grid =<< makeMove pos cmd
            where
                Grid pos _ _ = grid
        isFinalState state@AdventureGameState{..} =
            event `elem` [DiedOfThirst, DiedOfLava, DiedOfWorm, Won]

    instance GameConfig AdventureGameConfig AdventureGameState where
        initialState gc
            | isValidGameConfig gc = Just $ AdventureGameState {
                    grid = updateSeenGrid (toInteger $ sight gc) (initGrid gc),
                    event = ReplenishedWater,
                    water = waterCap gc,
                    tilesVisited = S.fromList [(0, 0)],
                    treasureCollected = S.empty,
                    wormsEmerging = S.empty,
                    wormsDisappearing = S.empty,
                    gameConfig = gc,
                    message = Nothing
                }
            | otherwise = Nothing
                
    -- given a game state, a list of tiles that are allowed to walk on and a list of goal tiles,
    -- it returns the distance (if there is a path) to the closest goal tile, by only walking over allowed tiles
    closestDistance :: (Num n, Ord n) => AdventureGameState -> [TileType] -> [TileType] -> Maybe n
    closestDistance AdventureGameState{..} allowedTiles goalTiles =
        cheapestBFS' pos (expand allowedTiles) (goalCheck goalTiles)        
        where
            Grid pos _ _ = grid
            expand allowedTiles coord = 
                filter (\c -> elem (tileType $ getTileAt grid c) allowedTiles) . catMaybes $ map (makeMove coord) [(UpMove)..(LeftMove)]
            goalCheck goalTiles coord =
                elem (tileType $ getTileAt grid coord) goalTiles
                
    -- checks if the game configuration has percentages that do not exceed 100
    isValidGameConfig :: AdventureGameConfig -> Bool
    isValidGameConfig AdventureGameConfig{..} =
        sight >= 0 &&
        waterCap > 0 &&
        treasurePct >= 0 &&
        treasurePct <= 100 &&
        waterPct > 0 &&
        portalPct > 0 &&
        lavaSinglePct >= 0 &&
        lavaAdjacentPct >= 0 &&
        wormLength > 0 &&
        wormSpawnPct >= 0 &&
        wormSpawnPct <= 100 &&
        waterPct + portalPct + lavaSinglePct <= 100 &&
        waterPct + portalPct + lavaAdjacentPct <= 100

    -- updates the game state given a new coordinate position by updating the event and by updating which new tiles have been discovered
    -- applies the event of the tile the player is on, based on the tile type (and the amount of water left)
    makeStateChange :: AdventureGameState -> Grid -> AdventureGameState
    makeStateChange state@AdventureGameState{..} g =
        newState {
            grid = if event == CollectedTreasure 
                   then setTileAt newGrid pos (GridTile (Desert False) True) -- remove treasure
                   else newGrid,
            event = event,
            water = if event == ReplenishedWater
                    then waterCap gameConfig
                    else water - 1,
            treasureCollected = if event == CollectedTreasure
                                then S.insert pos treasureCollected
                                else treasureCollected
        }
        where
            newGrid = updateSeenGrid (toInteger $ sight gameConfig) g
            Grid pos _ _ = newGrid
            newState = state {
                grid = newGrid,
                tilesVisited = S.insert pos tilesVisited
            }
            event = tileEvent newState

    tileEvent :: AdventureGameState -> AdventureGameEvent
    tileEvent AdventureGameState{..}
        | not (setListElem pos wormsEmerging && setListElem pos wormsDisappearing) = DiedOfWorm
        | tileT == Water = ReplenishedWater
        | tileT == Lava = DiedOfLava
        | tileT == Portal = Won
        | water == 0 = DiedOfThirst
        | tileT == Desert False = NoEvent
        | tileT == Desert True = CollectedTreasure
        where
            Grid pos tile _ = grid
            tileT = tileType tile
            setListElem e = S.null . S.filter (elem e) 
        

    -- initializes the grid zipper based on the game configuration, taking into account the seed and tile type percentages
    -- guarantees that the first tile is always a water tile
    initGrid :: AdventureGameConfig -> Grid
    initGrid gc@AdventureGameConfig{..} =
        Grid (0, 0) firstTile bcs
        where
            bcs = GridBreadcrumbs [] (initGridRows gc nextRng (firstTile : firstRow)) [] firstRow
            firstTile = GridTile Water True
            firstRow = initGridTiles gc (mkStdGen rowSeed) firstTile $ repeat firstTile
            (rowSeed, nextRng) = random $ mkStdGen seed

    -- iterates over the outer list of the infinite grid, takes the previous row as argument to be able to check if there was lava
    -- passes a new StdGen with new Int seed for each row, based on the initial seed
    initGridRows :: RandomGen g => AdventureGameConfig -> g -> GridRow -> [GridRow]
    initGridRows gc rng prevRow =
        nextRow : initGridRows gc nextRng nextRow
        where
            nextRow = initGridTiles gc (mkStdGen rowSeed) (GridTile Water False) prevRow
            (rowSeed, nextRng) = random rng

    -- generates an infinite row (inner list), takes the previous row and previous tile (in the row) as argument to check for lava
    initGridTiles :: RandomGen g => AdventureGameConfig -> g -> GridTile -> GridRow -> GridRow
    initGridTiles gc rng prevTile (prevRowTile : prevRow) =
                tile : initGridTiles gc nextRng tile prevRow
                where
                    (tile, nextRng) = initTile gc rng hasPrevLava
                    hasPrevLava = (tileType prevTile == Lava) || (tileType prevRowTile == Lava)

    -- given a RandomGen, it uses the percentages defined in the game config to generate a grid tile
    -- returns this grid tile and the next RandomGen
    initTile :: RandomGen g => AdventureGameConfig -> g -> Bool -> (GridTile, g)
    initTile AdventureGameConfig{..} rng isLavaAdjacent
        | rand < waterPct = (GridTile Water False, nextRng)
        | rand < portalThreshold = (GridTile Portal False, nextRng)
        | not isLavaAdjacent && rand < lavaSingleThreshold = (GridTile Lava False, nextRng)
        | isLavaAdjacent && rand < lavaAdjacentThreshold = (GridTile Lava False, nextRng)
        | otherwise = (GridTile (Desert (rand' < treasurePct)) False, nextRng')
        where
            portalThreshold = waterPct + portalPct
            lavaSingleThreshold = portalThreshold + lavaSinglePct
            lavaAdjacentThreshold = portalThreshold + lavaAdjacentPct
            (rand, nextRng)  = randomR (0 :: Double, 100) rng
            (rand', nextRng') = randomR (0 :: Double, 100) nextRng -- used to generate treasure