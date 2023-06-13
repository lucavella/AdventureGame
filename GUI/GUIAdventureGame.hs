{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.GUIAdventureGame (
    saveFile, handleEvent, renderState
) where

    import Models.Grid
    import Models.GameState
    import Models.AdventureGame
    import GUI.GUIGame
    import GUI.GUITiles
    import Utils.List
    import Persistence.Serializer
    import Persistence.Parser
    import Data.List
    import Data.Maybe
    import Graphics.Gloss.Data.Color
    import Graphics.Gloss.Interface.Environment
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    import System.Exit
    import Debug.Trace


    -- game save location
    saveFile :: String
    saveFile = "data.sav"

    -- transform picture to top left of given dimensions
    topLeft :: (Int, Int) -> Picture -> Picture
    topLeft (w, h) = translate (- fromIntegral w / 2) (fromIntegral h / 2)

    -- transform picture to top right
    topRight :: (Int, Int) -> Picture -> Picture
    topRight (w, h) = translate (fromIntegral w / 2) (fromIntegral h / 2)


    instance GUIGame AdventureGameState PlayerMove where
        handleEvent (EventKey (Char k) Down _ _) state =
            if isFinalState state -- no saving or moving on game over
            then case k of
                'n' -> new
                'l' -> load
                otherwise -> return state
            else case k of
                'w' -> move UpMove
                'd' -> move RightMove
                's' -> move DownMove
                'a' -> move LeftMove
                'n' -> new
                'k' -> save
                'l' -> load
                otherwise -> return state
            where
                move m = nextState state { message = Nothing } m >>= return . fromMaybe state
                save = saveGame saveFile state >> return state { message = Just "Game saved" }
                load = do
                    lg <- loadGame saveFile
                    case lg of
                        Just s -> return s { message = Just "Game loaded" }
                        Nothing -> return state { message = Just "Parse error" }
                new = do
                    g <- randomIO
                    case initialState $ (gameConfig state) { seed = g } of
                        Just s -> return s { message = Just "New game started" }
                        Nothing -> return state { message = Just "An error ocurred" }

        -- exit on ESC
        handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) state = exitSuccess
        handleEvent _ state = return state

        renderState state@AdventureGameState{..} = do
            (w, h) <- getScreenSize
            return $ pictures [
                topLeft (w, h) $ pictures [
                    -- game info
                    color white . translate 20 (-40) . scale 0.1 0.1 . Text $ "Position: " ++ show pos,
                    color white . translate 20 (-60) . scale 0.1 0.1 . Text $ "Water: " ++ show water ++ "/" ++ (show $ waterCap gameConfig),
                    color white . translate 20 (-80) . scale 0.1 0.1 . Text $ "Treasure: " ++ show (length treasureCollected),
                    color white . translate 20 (-100) . scale 0.1 0.1 . Text $ showDistances state,
                    color yellow . translate 20 (-120) . scale 0.1 0.1 . Text $ show event,
                    (case message of
                        Just m -> color red . translate 20 (-140) . scale 0.1 0.1 $ Text m
                        Nothing -> blank),
                    -- grid
                    translate 0 (-200) $ gridPicture state (w, h - 200) alive ],
                topRight (w, h) $ pictures [
                    -- key info
                    color white . translate (-200) (-40) . scale 0.1 0.1 $ Text "Movement: W A S D",
                    color white . translate (-200) (-60) . scale 0.1 0.1 $ Text "New game: N",
                    color white . translate (-200) (-80) . scale 0.1 0.1 $ Text "Save game: K",
                    color white . translate (-200) (-100) . scale 0.1 0.1 $ Text "Save game: L",
                    color white . translate (-200) (-120) . scale 0.1 0.1 $ Text "Quit game: ESC" ]]
            where
                Grid pos _ _ = grid
                alive = event `elem` [NoEvent, CollectedTreasure, ReplenishedWater, Won]


    -- gets the distance to the closest water, desert and portal tile and returns it as string
    showDistances :: AdventureGameState -> String
    showDistances state@AdventureGameState{..} = 
        "Closest: water(" ++ showMaybe waterDistance ++
        "), desert(" ++ showMaybe desertDistance ++
        "), portal(" ++ showMaybe portalDistance ++ ")"
        where
            waterDistance = closestDistance state [Water, DesertEmpty, DesertTreasure] [Water] -- cannot pass through portals to reach water
            desertDistance = closestDistance state [Water, DesertEmpty, DesertTreasure] [DesertEmpty, DesertTreasure] -- idem
            portalDistance = closestDistance state [Water, DesertEmpty, DesertTreasure, Portal] [Portal]
            showMaybe n = case n of
                Nothing -> "/"
                Just a -> show a


    -- gets subgrid around player position (including player and worms) as picture, given a width and height
    gridPicture :: AdventureGameState -> (Int, Int) -> Bool -> Picture
    gridPicture AdventureGameState{..} (w, h) alive =
        pictures $ gridPic ++ [wormsPic] ++ [playerPic]
        where
            tileSize = 25
            width = (fromIntegral w) / tileSize
            height = (fromIntegral h) / tileSize
            Grid (x, y) tile gbc = grid
            bmps = bmpTiles gameConfig

            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            lX = floor $ (width - 1) / 2 -- left x
            rX = ceiling $ (width - 1) / 2 -- right x
            tY = floor $ (height - 1) / 2 -- top y
            bY = ceiling $ (height - 1) / 2 -- bottom y

            oobTile = GridTile OutOfBounds True
            playerX = (fromIntegral lX) * tileSize
            playerY = (fromIntegral tY) * tileSize
            playerTile = if alive then PlayerAlive else PlayerDead
            playerPic = translate playerX (-playerY) $ getTilePicture bmps playerTile tileSize

            tPad = replicate (fromInteger $ tY - y) $ replicate (ceiling width) oobTile -- get top grid out of bound padding
            lPad = replicate (fromInteger $ lX - x) oobTile -- get left side out of bound padding

            r' = lPad ++ reverse (genericTake lX lr) ++ (tile : genericTake (rX + 1) rr) -- get current row
            tg' = tPad ++ reverseMap colFun (genericTake tY tg) -- get show of top grid
            bg' = map colFun (genericTake bY bg) -- get show of bottom grid

            colFun row = lPad ++ sublist (x - lX) (x + rX) row -- show row of tiles
            gridPictureFun i j t =
                translate (fromIntegral j * (tileSize)) (-fromIntegral i * (tileSize)) $ getGridTilePicture bmps t tileSize

            gridPic = concat . mapWithIndexMatrix gridPictureFun $ tg' ++ (r' : bg')
            wormsPic = wormsPicture bmps grid wormsEmerging wormsDisappearing (x, y) (lX, tY) tileSize

    -- gets worms picture, given emerging and disappearing worms, and grid (grid itself, location and offset)
    wormsPicture :: GUITiles -> Grid -> [[Coordinate]] -> [[Coordinate]] -> Coordinate -> Coordinate -> Float -> Picture
    wormsPicture bmps grid emerging disappearing (x, y) (xOffset, yOffset) size =
        pictures . concat $ emPic ++ disPic
        where
            trans (x', y') = translate ((fromIntegral (x' - x + xOffset)) * size) -- relative transform in subgrid, given coordinate
                                      (-(fromIntegral (y' - y + yOffset) * size))
            wormPicEm (h : b) = (h, trans h $ getTilePicture bmps WormHead size) : (wormPicB b) -- tuple of worm head picture with its coordinate
            wormPicB b = map (\c -> (c,  trans c $ getTilePicture bmps WormBody size)) b -- tupes of worm body picture with their coordinates
            isSeen (GridTile _ True) = True
            isSeen (GridTile _ False) = False
            l = trace ("offs: " ++ show xOffset ++ ", " ++ show yOffset ++ " | pos: " ++show x ++ ", " ++ show y) $ 3
            seenCheck ((x', y'), _) = (x - xOffset <= x') && -- left grid bound
                                      (y - yOffset <= y') && -- top grid bound
                                      (x + xOffset >= x') && -- right grid bound
                                      (y + yOffset >= y') && -- bottom grid bound
                                      (isSeen $ getTileAt grid (x', y')) -- keep worm pictrues that are on seen tiles
            filterSeen = l `seq` map snd . filter seenCheck
            
            emPic = map (filterSeen . wormPicEm) emerging
            disPic = map (filterSeen . wormPicB) disappearing