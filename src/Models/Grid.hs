{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.Grid (
    Coordinate, TileType(..), GridTile(..), GridRow, Grid(..), GridBreadcrumbs(..), PlayerMove(..), 
    makeMove, makeMoveGrid, getTileAt, updateSeenGrid, showGrid
) where

    import Utils.List
    import Data.List (genericIndex, genericTake, genericDrop, intercalate)
    import System.Random (mkStdGen, randomR)


    type Coordinate = (Integer, Integer)

    -- different displayable types of tiles, desert tiles also track if there is a treasure
    -- only the first 4 can actually appear in the grid, the others are used for displaying the grid
    data TileType = Desert Bool | Water | Lava | Portal | Player | Undiscovered | OutOfBounds
        deriving Eq

    -- data type to encapsulate both the type of a tile and if it has been discovered
    data GridTile = GridTile {
        tileType :: TileType,
        seen :: Bool
    }

    type GridRow = [GridTile]

    -- Grid implemented as zipper, keeping current coordinate and tile
    data Grid = Grid Coordinate GridTile GridBreadcrumbs
    -- GridBreadcrumbs stores top and bottom part of grid and left and right side of current row
    data GridBreadcrumbs = GridBreadcrumbs [GridRow] [GridRow] GridRow GridRow

    data PlayerMove = UpMove | RightMove | DownMove | LeftMove
        deriving (Eq, Enum)

    -- defines string representations corresponding to a tile type
    instance Show TileType where
        show tileType = case tileType of
            -- colored (unix) block representation
            Desert _ -> "\ESC[33m\x2591\x2591\ESC[0m"
            Water -> "\ESC[34m\x2592\x2592\ESC[0m"
            Lava -> "\ESC[31m\x2593\x2593\ESC[0m"
            Portal -> "\ESC[35m\x1fbbf \ESC[0m"
            Player -> "\ESC[32m\x1fbc5 \ESC[0m"
            Undiscovered -> "  "
            OutOfBounds -> "\x2588\x2588"

            -- alternatively, ASCII representation
            -- Desert _ -> "$"
            -- Water -> "~"
            -- Lava -> "/"
            -- Portal -> "O"
            -- Player -> "X"
            -- Undiscovered -> "#"
            -- OutOfBounds -> " "

    -- grid tiles are only shown as their actual type if they have been discovered
    instance Show GridTile where
        show (GridTile t True) = show t
        show (GridTile _ False) = show Undiscovered

    -- updates a coordinate by moving in direction, making sure not the go out of bounds
    makeMove :: Coordinate -> PlayerMove -> Maybe Coordinate
    makeMove (x, y) move = case move of
        DownMove -> Just (x, y + 1)
        RightMove -> Just (x + 1, y)
        UpMove -> if y > 0 then Just (x, y - 1) else Nothing
        LeftMove -> if x > 0 then Just (x - 1, y) else Nothing

    makeMoveGrid :: Grid -> PlayerMove -> Maybe Grid
    makeMoveGrid (Grid (x, y) tile gbc) DownMove =
        Just (Grid (x, y + 1) tile' gbc')
        where
            GridBreadcrumbs tg (r : bg) lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            r' = reverseAppend lr (tile : rr) -- restore old row
            (lr', tile' : rr') = reverseSplitAt x r -- split new row
            gbc' = GridBreadcrumbs (r' : tg) bg lr' rr'
    makeMoveGrid (Grid (x, y) tile gbc) RightMove =
        Just (Grid (x + 1, y) tile' gbc')
        where
            GridBreadcrumbs tg bg lr (tile' : rr) = gbc
            gbc' = GridBreadcrumbs tg bg (tile : lr) rr
    makeMoveGrid (Grid (x, y) tile gbc) UpMove =
        if y > 0 then Just (Grid (x, y - 1) tile' gbc') else Nothing
        where
            GridBreadcrumbs (r : tg) bg lr rr = gbc
            r' = reverseAppend lr (tile : rr)
            (lr', tile' : rr') = reverseSplitAt x r
            gbc' = GridBreadcrumbs tg (r' : bg) lr' rr'
    makeMoveGrid (Grid (x, y) tile gbc) LeftMove =
        if x > 0 then Just (Grid (x - 1, y) tile' gbc') else Nothing
        where
            GridBreadcrumbs tg bg (tile' : lr) rr = gbc
            gbc' = GridBreadcrumbs tg bg lr (tile : rr)


    -- Euclidean distance between 2 coordinates
    distance :: Coordinate -> Coordinate -> Double
    distance (x, y) (x', y') =
        sqrt $ fromInteger $ (x - x') ^ 2 + (y - y') ^ 2

    -- gets a grid tile based on a coordinate
    getTileAt :: Grid -> Coordinate -> GridTile
    getTileAt (Grid pos@(x, y) tile gbc) coord@(x', y')
        | x' < 0 = GridTile OutOfBounds True
        | y' < 0 = GridTile OutOfBounds True
        | pos == coord = tile -- current tile
        | y' == y && x' < x = genericIndex lr $ x - x' -- on current row, left side
        | y' == y && x' > x = genericIndex rr $ x' - x -- on current row, right side
        | y' < y = genericIndex (genericIndex tg $ y - y') x' -- top part grid
        | y' > y = genericIndex (genericIndex bg $ y' - y) x' -- bottom part grid
        where
            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
    
    -- updates the grid by marking all grid tiles as seen in a radius around a given coordinate
    -- only iterates over rectangle that encloses circle defined by radius
    updateSeenGrid :: Integer -> Grid -> Grid
    updateSeenGrid sight (Grid (x, y) tile gbc) =
        Grid (x, y) tile' gbc'
        where 
            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            gbc' = GridBreadcrumbs tg' bg' lr' rr'

            tile' = updateSeen 0 0 tile -- current relative position is (0, 0), relative to breadcrumb indices
            lr' = mapWithIndex (updateSeen 0) (genericTake sight lr) ++ genericDrop sight lr -- update left side current row
            rr' = mapWithIndex (updateSeen 0) (genericTake sight rr) ++ genericDrop sight rr -- update right side current row
            tg' = mapWithIndex rowFun (genericTake sight tg) ++ genericDrop sight tg -- update top part of grid
            bg' = mapWithIndex rowFun (genericTake sight bg) ++ genericDrop sight bg -- update bottom part of grid

            rowFun y row = -- only iterate over relevant part of row
                genericTake minX row ++ 
                mapWithIndex (updateSeen y) (sublist minX maxX row) ++ 
                genericDrop (maxX + 1) row
            minX = max 0 $ x - sight -- make sure not out of bounds
            maxX = x + sight

            updateSeen y x (GridTile t seen)
                | distance (x, y) (0, 0) <= fromIntegral sight = -- breadcrumb lists are centered around relative position (0, 0)
                    GridTile t True -- update if within sight radius
                | otherwise = GridTile t seen

    showGrid :: Grid -> Integer -> Integer -> String
    showGrid (Grid (x, y) tile gbc) width height =
        concat . concat $ tg' ++ (r' : bg') -- join shown tiles of top grid, current row and bottom grid
        where
            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            lX = floor $ fromIntegral (width - 1) / 2 -- left x
            rX = ceiling $ fromIntegral (width - 1) / 2 -- right x
            tY = floor $ fromIntegral (height - 1) / 2 -- top y
            bY = ceiling $ fromIntegral (height - 1) / 2 -- bottom y

            tPad = replicate (fromInteger $ tY - y) $ replicate (fromInteger width) obTile -- get top grid out of bound padding
            lPad = replicate (fromInteger $ lX - x) obTile -- get left side out of bound padding

            r' = lPad ++ reverseMap show (genericTake lX lr) ++ 
                 (pTile : map show (genericTake rX rr)) ++ ["\n"] -- get show of current row
            tg' = tPad ++ reverseMap colFun (genericTake tY tg) -- get show of top grid
            bg' = map colFun (genericTake bY bg) -- get show of bottom grid

            colFun row = lPad ++ map show (sublist (x - lX) (x + rX) row) ++ ["\n"] -- show row of tiles
            obTile = show $ GridTile OutOfBounds True
            pTile = show $ GridTile Player True