{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.Grid (
    Coordinate, TileType(..), GridTile(..), GridRow, Grid(..), GridBreadcrumbs(..), PlayerMove(..), 
    makeMove, makeMoveGrid, distance, getTileAt, setTileAt, updateSeenGrid
) where

    import Utils.List
    import Data.List


    type Coordinate = (Integer, Integer)

    -- different displayable types of tiles
    -- only the first 5 can actually appear in the grid
    data TileType = DesertEmpty
                  | DesertTreasure 
                  | Water 
                  | Lava 
                  | Portal 
                  | Undiscovered 
                  | OutOfBounds 
                  | PlayerAlive 
                  | PlayerDead 
                  | WormHead 
                  | WormBody
        deriving (Eq, Ord)

    -- data type to encapsulate both the type of a tile and if it has been discovered
    data GridTile = GridTile {
        tileType :: TileType,
        seen :: Bool
    }

    type GridRow = [GridTile]

    -- Grid implemented as zipper, keeping current coordinate and tile
    data Grid = Grid Coordinate GridTile GridBreadcrumbs
    -- grid zipper breadcrumbs store top and bottom part of grid and left and right side of current row
    data GridBreadcrumbs = GridBreadcrumbs [GridRow] [GridRow] GridRow GridRow

    data PlayerMove = UpMove | RightMove | DownMove | LeftMove
        deriving (Eq, Enum)

    -- updates a coordinate by moving in direction, making sure not the go out of bounds
    makeMove :: Coordinate -> PlayerMove -> Maybe Coordinate
    makeMove (x, y) move = case move of
        DownMove -> Just (x, y + 1)
        RightMove -> Just (x + 1, y)
        UpMove -> if y > 0 then Just (x, y - 1) else Nothing
        LeftMove -> if x > 0 then Just (x - 1, y) else Nothing

    -- updates a grid by moving the direction, making sure not to go out of bounds
    makeMoveGrid :: Grid -> Coordinate -> Maybe Grid
    makeMoveGrid g@(Grid pos@(x, y) tile gbc) coord@(x', y')
        | x' < 0 || y' < 0 = Nothing
        | pos == coord = Just g
        | y' == y && x' < x = 
            let (rr', tile' : lr') = reverseSplitAt (x - x') (tile : lr) -- split row at new x
            in Just . Grid coord tile' $ GridBreadcrumbs tg bg lr' (rr' ++ rr)
        | y' == y && x' > x =
            let (lr', tile' : rr') = reverseSplitAt (x' - x) (tile : rr) -- split row at new x
            in Just . Grid coord tile' $ GridBreadcrumbs tg bg (lr' ++ lr) rr'
        | y' < y =
            let r = reverseAppend lr (tile : rr) -- restore old row
                (bg', r' : tg') = reverseSplitAt (y - y') (r : tg) -- split top grid at new y
                (lr', tile' : rr') = reverseSplitAt x' r' -- split new row at new x
            in Just . Grid coord tile' $ GridBreadcrumbs tg' (bg' ++ bg) lr' rr'
        | y' > y =
            let r = reverseAppend lr (tile : rr) -- restore old row
                (tg', r' : bg') = reverseSplitAt (y' - y) (r : bg) -- split bottom grid at new y
                (lr', tile' : rr') = reverseSplitAt x' r' -- split new row at new x
            in Just . Grid coord tile' $ GridBreadcrumbs (tg' ++ tg) bg' lr' rr'
        where
            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow


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
        | y' == y && x' < x = genericIndex lr $ x - x' - 1 -- on current row, left side
        | y' == y && x' > x = genericIndex rr $ x' - x - 1 -- on current row, right side
        | y' < y = genericIndex (genericIndex tg $ y - y' - 1) x' -- top part grid
        | y' > y = genericIndex (genericIndex bg $ y' - y - 1) x' -- bottom part grid
        where
            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow

    -- sets a tile at a grid coordinate
    setTileAt :: Grid -> Coordinate -> GridTile -> Grid
    setTileAt g@(Grid pos@(x, y) tile gbc) coord@(x', y') newTile
        | x' < 0 || y' < 0 = g
        | pos == coord = Grid pos newTile gbc
        | y' == y && x' < x = Grid pos tile $ GridBreadcrumbs tg bg (genericSet (x - x' - 1) newTile lr) rr
        | y' == y && x' > x = Grid pos tile $ GridBreadcrumbs tg bg lr (genericSet (x' - x - 1) newTile rr)
        | y' < y = Grid pos tile $ GridBreadcrumbs (genericSet (y - y' - 1) tgr tg) bg lr rr
        | y' > y = Grid pos tile $ GridBreadcrumbs tg (genericSet (y' - y - 1) bgr bg) lr rr
        where
            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            tgr = genericSet x' newTile (genericIndex tg $ y - y' - 1)
            bgr = genericSet x' newTile (genericIndex bg $ y' - y - 1)
    
    -- updates the grid by marking all grid tiles as seen in a radius around a given coordinate
    -- only iterates over rectangle that encloses circle defined by radius
    updateSeenGrid :: Integer -> Grid -> Grid
    updateSeenGrid sight (Grid (x, y) tile gbc) =
        Grid (x, y) tile' gbc'
        where 
            GridBreadcrumbs tg bg lr rr = gbc -- topgrid, bottomgrid, leftrow, rightrow
            gbc' = GridBreadcrumbs tg' bg' lr' rr'

            tile' = updateSeen (0, 0) 0 0 tile
            lr' = mapWithIndex (updateSeen (-1, 0) 0) (genericTake sight lr) ++ genericDrop sight lr -- update left side current row, relative x pos is 1 before idx 0
            rr' = mapWithIndex (updateSeen (-1, 0) 0) (genericTake sight rr) ++ genericDrop sight rr -- update right side current row
            tg' = mapWithIndex rowFun (genericTake sight tg) ++ genericDrop sight tg -- update top part of grid
            bg' = mapWithIndex rowFun (genericTake sight bg) ++ genericDrop sight bg -- update bottom part of grid

            rowFun y row = -- only iterate over relevant part of row
                genericTake minX row ++ 
                mapWithIndex (updateSeen (x - minX, -1) y) (sublist minX maxX row) ++ -- relative x pos is x - removed columns, y pos is 1 before idx 0
                genericDrop (maxX + 1) row
            minX = max 0 $ x - sight -- make sure not out of bounds
            maxX = x + sight

            updateSeen relPos y x (GridTile t seen) -- relPos is relative position that is different for grids/rows/tile respectively
                | distance relPos (x, y) <= fromIntegral sight =
                    GridTile t True -- update if within sight radius
                | otherwise = GridTile t seen