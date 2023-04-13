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


    data AdventureGameConfig = AdventureGameConfig {
        seed :: Int,
        sight :: Int,
        waterCap :: Int,
        treasurePct :: Double,
        waterPct :: Double,
        portalPct :: Double,
        lavaSinglePct :: Double,
        lavaAdjacentPct :: Double,
        gridDim :: (Int, Int)
    }

    data AdventureGameState = AdventureGameState {
        position :: Coordinate,
        grid :: Grid,
        event :: AdventureGameEvent,
        water :: Int,
        treasure :: Int,
        gameConfig :: AdventureGameConfig
    }

    data AdventureGameEvent = NoEvent | CollectedTreasure | ReplenishedWater | DiedOfThirst | DiedOfLava | Won
        deriving Eq

    instance Read PlayerMove where
        readsPrec _ str = case map toLower str of
            "w" -> [(UpMove, "")]
            "a" -> [(LeftMove, "")]
            "s" -> [(DownMove, "")]
            "d" -> [(RightMove, "")]
            otherwise -> []

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

    instance Show AdventureGameState where
        show state@AdventureGameState{..} =
            "Water: " ++ show water ++ "/" ++ (show $ waterCap gameConfig) ++ 
            "\tTreasure: " ++ show treasure ++ "\n" ++
            showDistances state ++ "\n" ++
            show event ++ "\n" ++
            (intercalate "\n" $ map (intercalate "") tiles)
            where
                (width, height) = gridDim gameConfig
                (x, y) = position
                minX = x - (floor $ fromIntegral $ width `div` 2)
                minY = y - (floor $ fromIntegral $ height `div` 2)
                coords = [[(x, y) | x <- [minX..(minX + toInteger width)]] | y <- [minY..(minY + toInteger height)]]
                playerGrid = setTile grid position $ GridTile Player True
                tiles = map (map $ show . getTile playerGrid) coords

    showDistances :: AdventureGameState -> String
    showDistances state@AdventureGameState{..} = 
        "Closest: water(" ++ showMaybe waterDistance ++
        "), desert(" ++ showMaybe desertDistance ++
        "), portal(" ++ showMaybe portalDistance ++ ")"
        where
            waterDistance = closestDistance state [Water, Desert False, Desert True] [Water]
            desertDistance = closestDistance state [Water, Desert False, Desert True] [Desert False, Desert True]
            portalDistance = closestDistance state [Water, Desert False, Desert True, Portal] [Portal]
            showMaybe n = case n of
                Nothing -> "/"
                Just a -> show a
                
    closestDistance :: (Num n, Ord n) => AdventureGameState -> [TileType] -> [TileType] -> Maybe n
    closestDistance AdventureGameState{..} allowedTiles goalTiles =
        cheapestUC position (expand allowedTiles) (goalCheck goalTiles)        
        where
            expand allowedTiles coord = 
                filter (\c -> elem (tileType $ getTile grid c) allowedTiles) . catMaybes $ map (makeMove coord) [(UpMove)..(LeftMove)]
            goalCheck goalTiles coord =
                elem (tileType $ getTile grid coord) goalTiles
                
    isValidGameConfig :: AdventureGameConfig -> Bool
    isValidGameConfig AdventureGameConfig{..} =
        waterPortalPct + lavaSinglePct <= 100 &&
        waterPortalPct + lavaAdjacentPct <= 100 &&
        treasurePct <= 100
        where
            waterPortalPct = waterPct + portalPct

    makeStateChange :: AdventureGameState -> Coordinate -> Maybe AdventureGameState
    makeStateChange state@AdventureGameState{..} coord@(x, y)
        | x < 0 = Nothing
        | y < 0 = Nothing
        | otherwise = Just $ updateEvent state { position = coord, grid = newGrid }
            where
                newGrid = updateSeenGrid grid position (toInteger $ sight gameConfig)

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
            newGrid = setTile grid position $ GridTile (Desert False) True

    initGrid :: AdventureGameConfig -> Grid
    initGrid gc@AdventureGameConfig{..} =
        firstRow : initGridRows gc nextRng firstRow
        where
            firstTile = GridTile Water True
            firstRow = firstTile : (initGridTiles gc rowRng firstTile $ repeat firstTile)
            randomResult = random $ mkStdGen seed
            rowRng = mkStdGen $ fst randomResult
            nextRng = snd randomResult

    initGridRows :: RandomGen g => AdventureGameConfig -> g -> GridRow -> Grid
    initGridRows gc rng prevRow =
        nextRow : initGridRows gc nextRng nextRow
        where
            nextRow = initGridTiles gc rowRng (GridTile Water False) prevRow
            randomResult = random rng
            rowRng = mkStdGen $ fst randomResult
            nextRng = snd randomResult

    initGridTiles :: RandomGen g => AdventureGameConfig -> g -> GridTile -> GridRow -> GridRow
    initGridTiles gc rng prevTile (prevRowTile : prevRow) =
                tile : initGridTiles gc nextRng tile prevRow
                where
                    tileResult = initTile gc rng hasPrevLava
                    tile = fst tileResult
                    nextRng = snd tileResult
                    hasPrevLava = (tileType prevTile == Lava) || (tileType prevRowTile == Lava)

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
            randomResult' = randomR (0 :: Double, 100) nextRng
            rand' = fst randomResult
            nextRng' = snd randomResult'
