{-# LANGUAGE TupleSections #-}

module Models.Worm (
    updateWorms, spawnWorm
) where
    import Models.Grid
    import Data.List
    import Data.Maybe
    import Control.Monad
    import Control.Concurrent
    import Control.Concurrent.STM.TVar
    import Control.Monad.STM
    import System.Random


    type Worm = (Bool, [Coordinate]) -- keep track if worm is emerging
    type TWorm = TVar Worm
    type TTodo = TVar Int -- used to keep track of how many worms are busy


    -- transforms lists of emerging and disappearing worms into list of internal worm representation
    toWorms :: [[Coordinate]] -> [[Coordinate]] -> [Worm]
    toWorms emerging disappearing =
        map (True,) emerging ++ map (False,) disappearing

    -- undoes internal worm representation, back to lists of emerging and disappearing worms
    fromWorms :: [Worm] -> ([[Coordinate]], [[Coordinate]])
    fromWorms worms =
        (map snd em, map snd dis)
        where
            (em, dis) = partition fst worms

    -- reads list of worm TVars
    readTWorms :: [TWorm] -> STM [Worm]
    readTWorms = sequence . map readTVar

    -- start of third phase of worm lifecycle: emerging worms of maximum length become disappearing worms
    transitionWorms :: Int -> [Worm] -> [Worm]
    transitionWorms wormLength worms =
        map transWorm worms
        where
            transWorm (isEm, coords) = 
                if (isEm && length coords == wormLength)
                then (False, coords)
                else (isEm, coords)


    -- picks a move randomly, given a RandomGen, a list of coordinates for valid worm moves and current player coordiante
    -- stochastically moves towards player by favoring the closest possible coordinate towards the player
    chooseMove :: RandomGen g => [Coordinate] -> Coordinate -> g -> Coordinate
    chooseMove moveCoords userPos rng =
        (closestCoord : moveCoords) !! randIdx -- closest coordinate to player has twice the probability
        where
            (_, closestCoord) = minimum $ map (\c -> (distance c userPos, c)) moveCoords -- get the closes coordinate
            (randIdx, _) = randomR (0, length moveCoords) rng

    -- generates valid worm move coordinates, given the worm head coordinate, the grid and other worms in the game
    validMoves :: Coordinate -> Grid -> [Worm] -> [Coordinate]
    validMoves wormHead grid worms =
        filter (\c -> desertCoord c && notWormCoord c) moveCoords
        where
            moveCoords = catMaybes $ map (makeMove wormHead) [(UpMove)..(LeftMove)] -- get moves in all directions
            desertCoord = (==) DesertEmpty . tileType . getTileAt grid -- check if coordinate is desert without treasure
            notWormCoord c = all (notElem c) $ map snd worms -- check if no worm on coordinate


    -- third phase of worm lifecycle: worm continues to disappear by dropping its last body coordiante
    -- given the worm and todo worm count
    disappearWorm :: TWorm -> TTodo -> STM ()
    disappearWorm tWorm tTodo = do
        (_, wc) <- readTVar tWorm
        writeTVar tWorm (False, init wc)
        modifyTVar tTodo (\n -> n - 1) -- mark as done

    -- second phase of worm lifecycle: worm moves forward to valid tile
    -- given the worm, the grid, the other worms in the game, ...
    emergeWorm :: (RandomGen g) => TWorm -> Grid -> [TWorm] -> TTodo -> g -> STM ()
    emergeWorm tWorm grid tWorms tTodo rng = do
        (_, wc) <- readTVar tWorm
        worms <- readTWorms tWorms

        let h = head wc
            moveCoords = validMoves h grid worms

        if null moveCoords
        then disappearWorm tWorm tTodo -- transition to disappearing if no valid moves (phase 3)
        else do
            let c = chooseMove moveCoords pos rng
            writeTVar tWorm (True, c : wc) -- commit move
            isValidMove <- validMoveCheck c tWorm tWorms -- check if move was still valid
            check $ isValidMove -- retry transaction if move was invalid
            modifyTVar tTodo (\n -> n - 1) -- mark as done

        where
            Grid pos _ _ = grid

    
    -- checks if a move that was performed is still valid
    -- the worm itself is removed from the worm list because otherwise the move would always be invalid (move is contained in worm itself)
    -- other worms could not have modified this worm and valid moves takes this worm into account, so we can safely drop it
    validMoveCheck :: Coordinate -> TWorm -> [TWorm] -> STM Bool
    validMoveCheck c tWorm tWorms = do
        worms' <- readTWorms tWorms'
        return . all (notElem c) $ map snd worms'
        where
            tWorms' = filter (/= tWorm) tWorms -- drop worm itself


    -- forks worm update procedure and atomically performs appropriate transaction
    updateWorm :: TWorm -> Grid -> [TWorm] -> TTodo -> IO ThreadId
    updateWorm tWorm grid tWorms tTodo =
        forkIO $ do
            seed <- randomIO
            atomically $ do
                worm <- readTVar tWorm
                case worm of
                    (True, _) -> emergeWorm tWorm grid tWorms tTodo (mkStdGen seed)
                    (False, _) -> disappearWorm tWorm tTodo


    -- concurrently update worms, given the grid, worm length and lists of emerging and disappearing worms
    updateWorms :: Int -> Grid -> [[Coordinate]] -> [[Coordinate]] -> IO ([[Coordinate]], [[Coordinate]])
    updateWorms wormLength grid emerging disappearing = do
        tWorms <- sequence $ map newTVarIO worms -- worm TVars
        tTodo <- newTVarIO $ length worms -- worm count as todo counter
        forM_ tWorms $ \tw -> updateWorm tw grid tWorms tTodo -- update worm in new thread
            
        atomically $ do
            todo <- readTVar tTodo
            check $ todo == 0 -- check if all done
            worms <- readTWorms tWorms
            let (emerging', disappearing') = fromWorms worms
            return (emerging', filter (not . null) disappearing') -- return updated worms, discard any worm that completly disappeared

        where
            worms = transitionWorms wormLength $ toWorms emerging disappearing


    -- first phase of worm lifecycle: randomly allow worm to spawn on valid tile
    -- given grid and lists of emerging and disappearing worms (to spawn on valid tiles)
    -- and spawn area parameters to control min and max spawn range from player
    spawnWorm :: Grid -> [[Coordinate]] -> [[Coordinate]] -> Double -> Double -> Int -> Int -> IO (Maybe Coordinate)
    spawnWorm grid emerging disappearing spawnRate minSpawnDistance spawnHeight spawnWidth = do
        spawnRand <- randomRIO (0, 100)
        if (spawnRand < spawnRate) && (not $ null coords')
        then do
            idx <- randomRIO (0, length coords' - 1)
            return . Just $ coords' !! idx
        else return Nothing

        where
            Grid pos@(x, y) _ _ = grid
            offsetX = floor $ (fromIntegral spawnWidth - 1) / 2
            offsetY = floor $ (fromIntegral spawnHeight - 1) / 2
            coords = do
                { x' <- [(x - offsetX)..(x + offsetX)]
                ; y' <- [(y - offsetY)..(y + offsetY)]
                ; return (x', y') }
            coords' = filter (\c -> isMinDistance c && hasNoWorm c && isDesertEmpty c) coords

            isMinDistance c = distance c pos >= minSpawnDistance
            hasNoWorm c = (all (notElem c) emerging) && (all (notElem c) disappearing)
            isDesertEmpty c = (tileType $ getTileAt grid c) == DesertEmpty
