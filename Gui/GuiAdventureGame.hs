{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Gui.GuiAdventureGame where
    import Models.Grid
    import Models.GameState
    import Models.AdventureGame
    import Gui.GuiGame
    import Utils.List
    import Persistence.Serializer
    import Persistence.Parser
    import Data.List
    import Data.Maybe
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Data.Color
    import Graphics.Gloss.Interface.Environment
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    import System.Exit


    saveFile :: String
    saveFile = "data.sav"

    tileSize :: Int
    tileSize = 10


    topLeft :: (Int, Int) -> Picture -> Picture
    topLeft (w, h) = translate (- fromIntegral w / 2) (fromIntegral h / 2)

    topRight :: (Int, Int) -> Picture -> Picture
    topRight (w, h) = translate (fromIntegral w / 2) (fromIntegral h / 2)


    instance GuiGame AdventureGameState PlayerMove where
        handleEvent (EventKey (Char k) Down _ _) state =
            if isFinalState state
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
                move = return . fromMaybe state . nextState state { message = Nothing }
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

        handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) state = exitSuccess
        handleEvent _ state = return state

        renderMap state@AdventureGameState{..} = do
            (w, h) <- getScreenSize
            gridPic <- gridPicture grid (w, h - 200) alive
            return $ pictures [
                topLeft (w, h) $ pictures [
                    color white . translate 20 (-40) . scale 0.1 0.1 . Text $ "Position: " ++ show pos,
                    color white . translate 20 (-60) . scale 0.1 0.1 . Text $ "Water: " ++ show water ++ "/" ++ (show $ waterCap gameConfig),
                    color white . translate 20 (-80) . scale 0.1 0.1 . Text $ "Treasure: " ++ show (length treasureCollected),
                    color white . translate 20 (-100) . scale 0.1 0.1 . Text $ showDistances state,
                    color yellow . translate 20 (-120) . scale 0.1 0.1 . Text $ show event,
                    (case message of
                        Just m -> color red . translate 20 (-140) . scale 0.1 0.1 $ Text m
                        Nothing -> blank),
                    translate 0 (-200) gridPic ],
                topRight (w, h) $ pictures [
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
            waterDistance = closestDistance state [Water, Desert False, Desert True] [Water] -- cannot pass through portals to reach water
            desertDistance = closestDistance state [Water, Desert False, Desert True] [Desert False, Desert True] -- idem
            portalDistance = closestDistance state [Water, Desert False, Desert True, Portal] [Portal]
            showMaybe n = case n of
                Nothing -> "/"
                Just a -> show a


    -- show subgrid around player position, given a width and height
    gridPicture :: Grid -> (Int, Int) -> Bool -> IO Picture
    gridPicture (Grid (x, y) tile gbc) (w, h) alive =
        fmap pictures . sequence $ gridPics ++ [playerPic]
        where
            tileSize = 25
            playerRadius = 10
            width = (fromIntegral w) / tileSize
            height = (fromIntegral h) / tileSize

            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            lX = floor $ (width - 1) / 2 -- left x
            rX = ceiling $ (width - 1) / 2 -- right x
            tY = floor $ (height - 1) / 2 -- top y
            bY = ceiling $ (height - 1) / 2 -- bottom y

            oobTile = GridTile OutOfBounds True
            playerX = (fromIntegral lX) * (tileSize) + (tileSize + 1) / 2
            playerY = (fromIntegral tY) * (tileSize) + tileSize / 2
            playerPic = fmap (translate playerX (-playerY)) $ playerPicture alive

            tPad = replicate (fromInteger $ tY - y) $ replicate (ceiling width) oobTile -- get top grid out of bound padding
            lPad = replicate (fromInteger $ lX - x) oobTile -- get left side out of bound padding

            r' = lPad ++ reverse (genericTake lX lr) ++ (tile : genericTake (rX + 1) rr) -- get current row
            tg' = tPad ++ reverseMap colFun (genericTake tY tg) -- get show of top grid
            bg' = map colFun (genericTake bY bg) -- get show of bottom grid

            colFun row = lPad ++ sublist (x - lX) (x + rX) row -- show row of tiles
            gridPictureFun i j t =
                fmap (translate (fromIntegral j * (tileSize)) (-fromIntegral i * (tileSize))) $ tilePicture tileSize t

            gridPics = concat . mapWithIndexMatrix gridPictureFun $ tg' ++ (r' : bg')


    tilePicture :: Float -> GridTile -> IO Picture
    tilePicture size tile = 
        case tile of
            GridTile _ False -> return . Color g . translate 1 (-1) $ polygon [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
            GridTile t True -> case t of
                Desert _ -> fmap (translate sx (-sy)) $ loadBMP "Images/desert.bmp"
                Water -> fmap (translate sx (-sy)) $ loadBMP "Images/water.bmp"
                Lava -> fmap (translate sx (-sy)) $ loadBMP "Images/lava.bmp"
                Portal -> fmap (translate sx (-sy)) $ loadBMP "Images/portal.bmp"
                OutOfBounds -> return . Color k . translate 1 (-1) $ polygon [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
        where
            x1 = 0
            x2 = x1 + size
            y1 = 0
            y2 = y1 - size
            sx = size / 2 + 1
            sy = size / 2
            k = makeColorI 0 0 0 255
            g = makeColorI 38 38 38 255

    playerPicture :: Bool -> IO Picture
    playerPicture alive = loadBMP $
        if alive
        then "Images/alive.bmp"
        else "Images/dead.bmp"
        