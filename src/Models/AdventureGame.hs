{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.AdventureGame (
    AdventureGameConfig(..), AdventureGameState(..), 
    isValidGameConfig, closestDistance, initGrid
) where

    import Models.Grid
    import Models.TerminalGame
    import Utils.List (mapWithIndex, sublist, setAtIndex)
    import Utils.UniformCostSearch (cheapestUC)
    import Data.Char (toLower)
    import Data.Maybe (catMaybes)
    import System.Random (StdGen, RandomGen, mkStdGen, random, randomR)
    import Text.Read (ReadS, readMaybe)


    -- Game configuration data type that defines a unique initial game state
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
        gridDim :: (Int, Int)
    }

    -- Game state data type that contains all information at a given point in the game
    data AdventureGameState = AdventureGameState {
        grid :: Grid,
        event :: AdventureGameEvent,
        water :: Int,
        treasure :: Int,
        gameConfig :: AdventureGameConfig
    }

    -- Data type used in the game's state to keep track of what happened
    data AdventureGameEvent = NoEvent | CollectedTreasure | ReplenishedWater | DiedOfThirst | DiedOfLava | Won
        deriving Eq

    -- PlayerMove is made a member of Read, so a user command (string) can easily be translated to a move
    instance Read PlayerMove where
        readsPrec _ str = case map toLower str of
            "w" -> [(UpMove, "")]
            "a" -> [(LeftMove, "")]
            "s" -> [(DownMove, "")]
            "d" -> [(RightMove, "")]
            otherwise -> []
    
    -- AdventureGameState is made a GameState and TerminalGame instance so it can be used in runGame
    instance GameState AdventureGameState where
        nextState state@AdventureGameState{..} cmd =
            pure . (makeStateChange state) =<< makeMoveGrid grid =<< readMaybe cmd
        isFinalState state@AdventureGameState{..} =
            event `elem` [DiedOfThirst, DiedOfLava, Won]

    instance TerminalGame AdventureGameState AdventureGameConfig where
        initialState gc
            | isValidGameConfig gc = Right $ AdventureGameState {
                    grid = updateSeenGrid (toInteger $ sight gc) (initGrid gc),
                    event = ReplenishedWater,
                    water = waterCap gc,
                    treasure = 0,
                    gameConfig = gc
                }
            | otherwise = Left "Invalid configuration parameters"

    instance Show AdventureGameEvent where
        show event = case event of
            NoEvent -> "You roam an empty and barren land."
            CollectedTreasure -> "You found a treasure!"
            ReplenishedWater -> "You refilled your water bottle."
            DiedOfThirst -> "You died of thirst."
            DiedOfLava -> "You died from lava."
            Won -> "You safely escaped with all collected treasure!"

    -- AdventureGameState is displayed by showing position, water, treasure, the distances to relevant tiles and the grid
    instance Show AdventureGameState where
        show state@AdventureGameState{..} =
            "Position: " ++ show pos ++ "   " ++
            "Water: " ++ show water ++ "/" ++ (show $ waterCap gameConfig) ++ "   " ++
            "Treasure: " ++ show treasure ++ "\n" ++
            showDistances state ++ "\n" ++
            show event ++ "\n" ++
            showGrid grid (toInteger width) (toInteger height)
            where
                Grid pos _ _ = grid
                (width, height) = gridDim gameConfig

    -- gets the distance to the closest water, desert and portal tile and returns it as string
    showDistances :: AdventureGameState -> String
    showDistances state@AdventureGameState{..} = 
        "Closest: water(" ++ showMaybe waterDistance ++
        "), desert(" ++ showMaybe desertDistance ++
        "), portal(" ++ showMaybe portalDistance ++ ")"
        where
            waterDistance = closestDistance state [Water, Desert False, Desert True] [Water] -- cannot pass through portals to reach water
            desertDistance = closestDistance state [Water, Desert False, Desert True] [Desert False, Desert True] -- idem
            portalDistance = closestDistance state [Water, Desert False, Desert True, Portal] [Portal]
            showMaybe n = case n of
                Nothing -> "/"
                Just a -> show a
                
    -- given a game state, a list of tiles that are allowed to walk on and a list of goal tiles,
    -- it returns the distance (if there is a path) to the closest goal tile, by only walking over allowed tiles
    closestDistance :: (Num n, Ord n) => AdventureGameState -> [TileType] -> [TileType] -> Maybe n
    closestDistance AdventureGameState{..} allowedTiles goalTiles =
        cheapestUC pos (expand allowedTiles) (goalCheck goalTiles)        
        where
            Grid pos _ _ = grid
            expand allowedTiles coord = 
                filter (\c -> elem (tileType $ getTileAt grid c) allowedTiles) . catMaybes $ map (makeMove coord) [(UpMove)..(LeftMove)]
            goalCheck goalTiles coord =
                elem (tileType $ getTileAt grid coord) goalTiles
                
    -- checks if the game configuration has percentages that do not exceed 100
    isValidGameConfig :: AdventureGameConfig -> Bool
    isValidGameConfig AdventureGameConfig{..} =
        waterPortalPct + lavaSinglePct <= 100 &&
        waterPortalPct + lavaAdjacentPct <= 100 &&
        treasurePct <= 100
        where
            waterPortalPct = waterPct + portalPct

    -- updates the game state given a new coordinate position by updating the event and by updating which new tiles have been discovered
    -- applies the event of the tile the player is on, based on the tile type (and the amount of water left)
    makeStateChange :: AdventureGameState -> Grid -> AdventureGameState
    makeStateChange state@AdventureGameState{..} gr
        | tileT == Water = state { event = ReplenishedWater, water = waterCap gameConfig, grid = newGrid }
        | tileT == Lava = state { event = DiedOfLava, grid = newGrid }
        | tileT == Portal = state { event = Won, grid = newGrid }
        | water == 0 = state { event = DiedOfThirst, grid = newGrid }
        | tileT == Desert False = state { event = NoEvent, water = water - 1, grid = newGrid }
        | tileT == Desert True = state { event = CollectedTreasure, water = water - 1, treasure = treasure + 1, grid = newGrid' }
        where
            newGrid@(Grid pos tile gbc) = updateSeenGrid (toInteger $ sight gameConfig) gr
            tileT = tileType $ getTileAt newGrid pos
            newGrid' = Grid pos (GridTile (Desert False) True) gbc -- remove treasure from desert

    -- initializes the grid based on the game configuration, taking into account the seed and tile type percentages
    -- guarantees that the first tile is always a water tile
    initGrid :: AdventureGameConfig -> Grid
    initGrid gc@AdventureGameConfig{..} =
        Grid (0, 0) firstTile $ GridBreadcrumbs [[]] (initGridRows gc nextRng $ firstTile : firstRow) [] firstRow
        where
            firstTile = GridTile Water True
            firstRow = initGridTiles gc rowRng firstTile $ repeat firstTile
            randomResult = random $ mkStdGen seed
            rowRng = mkStdGen $ fst randomResult
            nextRng = snd randomResult

    -- iterates over the outer list of the infinite grid, takes the previous row as argument to be able to check if there was lava
    -- passes a new StdGen with new Int seed for each row, based on the initial seed
    initGridRows :: RandomGen g => AdventureGameConfig -> g -> GridRow -> [GridRow]
    initGridRows gc rng prevRow =
        nextRow : initGridRows gc nextRng nextRow
        where
            nextRow = initGridTiles gc rowRng (GridTile Water False) prevRow
            randomResult = random rng
            rowRng = mkStdGen $ fst randomResult
            nextRng = snd randomResult

    -- generates an infinite row (inner list), takes the previous row and previous tile (in the row) as argument to check for lava
    initGridTiles :: RandomGen g => AdventureGameConfig -> g -> GridTile -> GridRow -> GridRow
    initGridTiles gc rng prevTile (prevRowTile : prevRow) =
                tile : initGridTiles gc nextRng tile prevRow
                where
                    tileResult = initTile gc rng hasPrevLava
                    tile = fst tileResult
                    nextRng = snd tileResult
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
            randomResult  = randomR (0 :: Double, 100) rng
            rand = fst randomResult
            nextRng = snd randomResult
            randomResult' = randomR (0 :: Double, 100) nextRng -- used to generate treasure
            rand' = fst randomResult
            nextRng' = snd randomResult'
