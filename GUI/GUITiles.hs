module GUI.GUITiles (
    GUITiles(..),
    loadBMPTiles, getTilePicture, getGridTilePicture
) where

    import Models.Grid
    import qualified Data.Map as M
    import Data.Maybe
    import Data.Either
    import Graphics.Gloss.Data.Picture
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Data.Color
    import Codec.BMP

    type GUITiles = M.Map TileType BMP


    -- loads tile bitmap images into Map so that every renderState call does not need these additional IO operations
    loadBMPTiles :: IO GUITiles
    loadBMPTiles = do
        alive <- readBMP "Images/alive.bmp"
        dead <- readBMP "Images/dead.bmp"
        desert <- readBMP "Images/desert.bmp"
        lava <- readBMP "Images/lava.bmp"
        portal <- readBMP "Images/portal.bmp"
        water <- readBMP "Images/water.bmp"
        wormb <- readBMP "Images/wormb.bmp"
        wormh <- readBMP "Images/wormh.bmp"

        let bmps = [ (PlayerAlive, alive)
                   , (PlayerDead, dead)
                   , (DesertEmpty, desert)
                   , (DesertTreasure, desert)
                   , (Lava, lava)
                   , (Portal, portal)
                   , (Water, water)
                   , (WormBody, wormb)
                   , (WormHead, wormh) ]

        return . M.fromList . map getRight $ filter (isRight . snd) bmps -- only keep successful loads

        where
            getRight (t, (Right r)) = (t, r)


    -- gets bitmap of tile as picture, or shape as fallback
    getTilePicture :: GUITiles -> TileType -> Float -> Picture
    getTilePicture bmpTiles t size = case t of
        PlayerAlive -> maybe (playerTrans $ color playerColor playerCirc) (playerTrans . bitmapOfBMP) $ M.lookup PlayerAlive bmpTiles
        PlayerDead -> maybe (playerTrans $ color playerColor playerCirc) (playerTrans . bitmapOfBMP) $ M.lookup PlayerDead bmpTiles
        DesertEmpty -> maybe (color desertColor rect) (tileTrans . bitmapOfBMP) $ M.lookup DesertEmpty bmpTiles
        DesertTreasure -> maybe (color desertColor rect) (tileTrans . bitmapOfBMP) $ M.lookup DesertTreasure bmpTiles
        Lava -> maybe (color lavaColor rect) (tileTrans . bitmapOfBMP) $ M.lookup Lava bmpTiles
        Portal -> maybe (color portalColor rect) (tileTrans . bitmapOfBMP) $ M.lookup Portal bmpTiles
        Water -> maybe (color waterColor rect) (tileTrans . bitmapOfBMP) $ M.lookup Water bmpTiles
        WormBody -> maybe (tileTrans $ color wormBColor circ) (tileTrans . bitmapOfBMP) $ M.lookup WormBody bmpTiles
        WormHead -> maybe (tileTrans $ color wormHColor circ) (tileTrans . bitmapOfBMP) $ M.lookup WormHead bmpTiles
        Undiscovered -> color undiscColor rect
        OutOfBounds -> color oobColor rect
    
        where
            playerTrans = translate ((size - 1) / 2) (-size / 2)
            tileTrans = translate (size / 2) (-size / 2)
            rect = polygon [(0, 0), (size, 0), (size, -size), (0, -size)]
            playerCirc = circleSolid $ size / 3
            circ = circleSolid $ size / 2
            playerColor = makeColorI 41 157 34 255
            desertColor = makeColorI 254 216 131 255
            lavaColor = makeColorI 246 114 10 255
            portalColor = makeColorI 168 54 209 255
            waterColor = makeColorI 0 113 193 255
            wormBColor = makeColorI 120 109 87 255
            wormHColor = makeColorI 38 43 41 255
            undiscColor = makeColorI 38 38 38 255
            oobColor = makeColorI 0 0 0 255

    -- getTilePicture wrapper for GridTiles
    getGridTilePicture :: GUITiles -> GridTile -> Float -> Picture
    getGridTilePicture bmps tile size = 
        case tile of
            GridTile _ False -> getTilePicture bmps Undiscovered size
            GridTile t True -> getTilePicture bmps t size