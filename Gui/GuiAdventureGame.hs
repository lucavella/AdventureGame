{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Gui.GuiAdventureGame where
    import Models.Grid
    import Models.GameState
    import Models.AdventureGame
    import Gui.GuiGame
    import Utils.List
    import Data.List
    import Graphics.Gloss
    import Graphics.Gloss.Data.Picture
    import Graphics.Gloss.Data.Color
    import Graphics.Gloss.Interface.Environment
    import Graphics.Gloss.Interface.IO.Game
    import System.Exit


    tileSize :: Int
    tileSize = 10


    topLeft :: (Int, Int) -> Picture -> Picture
    topLeft (w, h) = translate (- fromIntegral w / 2) (fromIntegral h / 2) -----------

    topRight :: (Int, Int) -> Picture -> Picture
    topRight (w, h) = translate (fromIntegral w / 2) (fromIntegral h / 2) ------------


    instance GuiGame AdventureGameState PlayerMove where
        handleEvent (EventKey (Char k) Down _ _) state =
            return $
                if isFinalState state
                then state
                else case k of
                    'w' -> newState UpMove
                    'd' -> newState RightMove
                    's' -> newState DownMove
                    'a' -> newState LeftMove
                    otherwise -> state
            where
                newState = maybe state id . nextState state
        handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) state = exitSuccess
        handleEvent _ state = return state

        renderMap state@AdventureGameState{..} = do
            (w, h) <- getScreenSize
            return $ pictures [
                topLeft (w, h) $ pictures [
                    color white . translate 20 (-40) . scale 0.1 0.1 . Text $ "Position: " ++ show pos,
                    color white . translate 20 (-60) . scale 0.1 0.1 . Text $ "Water: " ++ show water ++ "/" ++ (show $ waterCap gameConfig),
                    color white . translate 20 (-80) . scale 0.1 0.1 . Text $ "Treasure: " ++ show (length treasureCollected),
                    color white . translate 20 (-100) . scale 0.1 0.1 . Text $ showDistances state,
                    color white . translate 20 (-120) . scale 0.1 0.1 . Text $ show event,
                    color white . translate 0 (-200) $ gridPicture grid (w, h - 200) ],
                topRight (w, h) $ pictures [
                    color white . translate (-200) (-40) . scale 0.1 0.1 $ Text "Controls",
                    color white . translate (-200) (-60) . scale 0.1 0.1 $ Text "Movement: W A S D",
                    color white . translate (-200) (-80) . scale 0.1 0.1 $ Text "Save: K",
                    color white . translate (-200) (-100) . scale 0.1 0.1 $ Text "Quit: ESC" ]]
            where
                Grid pos _ _ = grid


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
    gridPicture :: Grid -> (Int, Int) -> Picture
    gridPicture (Grid (x, y) tile gbc) (w, h) =
        pictures (playerPic : concat . mapWithIndexMatrix gridPictureFun $ tg' ++ (r' : bg'))
        --Text $ show $ length $ concat $ mapWithIndexMatrix gridPictureFun $ tg' ++ (r' : bg')
        where
            tileSize = 24
            playerRadius = 10
            width = (fromIntegral w) / tileSize
            height = (fromIntegral h) / tileSize

            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            lX = floor $ width / 2 -- left x
            rX = ceiling $ width / 2 -- right x
            tY = floor $ height / 2 -- top y
            bY = ceiling $ height / 2 -- bottom y

            oobTile = GridTile OutOfBounds True
            playerTile = GridTile Player True

            tPad = replicate (fromInteger $ tY - y) $ replicate (ceiling width) oobTile -- get top grid out of bound padding
            lPad = replicate (fromInteger $ lX - x) oobTile -- get left side out of bound padding

            r' = lPad ++ reverse (genericTake lX lr) ++ (tile : genericTake (rX + 1) rr) -- get current row
            tg' = tPad ++ reverseMap colFun (genericTake tY tg) -- get show of top grid
            bg' = map colFun (genericTake bY bg) -- get show of bottom grid

            playerTileX = lX * tileSize + (tileSize / 2) - playerRadius
            playerTileY = -tY * tileSize - (tileSize / 2) + playerRadius
            playerPic = translate playerTileX playerTileY . color (makeColorI 110 71 5 255) $ circle playerRadius

            colFun row = lPad ++ sublist (x - lX) (x + rX) row -- show row of tiles

            gridPictureFun i j t =
                translate (fromIntegral j * tileSize) (-fromIntegral i * tileSize) $ tilePicture tileSize t

    tilePicture :: Float -> GridTile -> Picture
    tilePicture size tile = 
        color c $ polygon [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
        where
            x1 = 0
            x2 = x1 + size
            y1 = 0
            y2 = y1 - size
            c = case tile of
                GridTile _ False -> makeColorI 38 38 38 255
                GridTile t True -> case t of
                    Desert _ -> makeColorI 89 88 34 255
                    Water -> makeColorI 35 55 89 255
                    Lava -> makeColorI 82 23 21 255
                    Portal -> makeColorI 66 21 82 255
                    OutOfBounds -> makeColorI 0 0 0 255