{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.AdventureGame (AdventureGameConfig(..), AdventureGameState(..), isValidGameConfig, closestDistance, initGrid) where

    import Models.Grid
    import Models.TerminalGame
    import Utils.List (mapWithIndex, sublist, setAtIndex)
    import Utils.UniformCostSearch (cheapestUC)
    import Data.Char (toLower)
    import Data.Maybe (catMaybes)
    import Data.List (intercalate)
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
        position :: Coordinate,
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
            makeStateChange state =<< makeMove position =<< readMaybe cmd
        isFinalState state@AdventureGameState{..} =
            event `elem` [DiedOfThirst, DiedOfLava, Won]

    instance TerminalGame AdventureGameState AdventureGameConfig where
        initialState gc
            | isValidGameConfig gc = Right $ AdventureGameState {
                    position = (0, 0),
                    grid = updateSeenGrid (initGrid gc) (0, 0) (toInteger $ sight gc),
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
            "Position: " ++ show position ++ "   " ++
            "Water: " ++ show water ++ "/" ++ (show $ waterCap gameConfig) ++ "   " ++
            "Treasure: " ++ show treasure ++ "\n" ++
            showDistances state ++ "\n" ++
            show event ++ "\n" ++
            (intercalate "\n" $ map (intercalate "") tiles) -- the tiles within a grid row are appended and the rows are joined with a newline
            where
                (width, height) = gridDim gameConfig
                (x, y) = position
                minX = x - (floor $ fromIntegral $ width `div` 2) -- min x coordinate to display
                minY = y - (floor $ fromIntegral $ height `div` 2) -- min y coordinate to display
                coords = [[(x, y) | x <- [minX..(minX + toInteger width)]] | y <- [minY..(minY + toInteger height)]] -- (x, y) nested list of coordinates to display
                playerGrid = setTile grid position $ GridTile Player True -- set the player tile at the player's position in the grid
                tiles = map (map $ show . getTile playerGrid) coords -- get the string representation of each tile

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
        cheapestUC position (expand allowedTiles) (goalCheck goalTiles)        
        where
            expand allowedTiles coord = 
                filter (\c -> elem (tileType $ getTile grid c) allowedTiles) . catMaybes $ map (makeMove coord) [(UpMove)..(LeftMove)]
            goalCheck goalTiles coord =
                elem (tileType $ getTile grid coord) goalTiles
                
    -- checks if the game configuration has percentages that do not exceed 100
    isValidGameConfig :: AdventureGameConfig -> Bool
    isValidGameConfig AdventureGameConfig{..} =
        waterPortalPct + lavaSinglePct <= 100 &&
        waterPortalPct + lavaAdjacentPct <= 100 &&
        treasurePct <= 100
        where
            waterPortalPct = waterPct + portalPct

    -- updates the game state given a new coordinate position by updating the event and by updating which new tiles have been discovered
    makeStateChange :: AdventureGameState -> Coordinate -> Maybe AdventureGameState
    makeStateChange state@AdventureGameState{..} coord@(x, y)
        | x < 0 = Nothing
        | y < 0 = Nothing
        | otherwise = Just $ updateEvent state { position = coord, grid = newGrid }
            where
                newGrid = updateSeenGrid grid position (toInteger $ sight gameConfig)

    -- applies the event of the tile the player is on, based on the tile type (and the amount of water left)
    updateEvent :: AdventureGameState -> AdventureGameState
    updateEvent state@AdventureGameState{..}
        | tile == Water = state { event = ReplenishedWater, water = waterCap gameConfig }
        | tile == Lava = state { event = DiedOfLava }
        | tile == Portal = state { event = Won }
        | water == 0 = state { event = DiedOfThirst }
        | tile == Desert False = state { event = NoEvent, water = water - 1 }
        | tile == Desert True = state { event = CollectedTreasure, grid = newGrid, water = water - 1, treasure = treasure + 1 }
        where
            tile = tileType $ getTile grid position
            newGrid = setTile grid position $ GridTile (Desert False) True -- remove treasure from desert

    -- initializes the grid based on the game configuration, taking into account the seed and tile type percentages
    -- guarantees that the first tile is always a water tile
    initGrid :: AdventureGameConfig -> Grid
    initGrid gc@AdventureGameConfig{..} =
        firstRow : initGridRows gc nextRng firstRow
        where
            firstTile = GridTile Water True
            firstRow = firstTile : (initGridTiles gc rowRng firstTile $ repeat firstTile)
            randomResult = random $ mkStdGen seed
            rowRng = mkStdGen $ fst randomResult
            nextRng = snd randomResult

    -- iterates over the outer list of the infinite grid, takes the previous row as argument to be able to check if there was lava
    -- passes a new StdGen with new Int seed for each row, based on the initial seed
    initGridRows :: RandomGen g => AdventureGameConfig -> g -> GridRow -> Grid
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
