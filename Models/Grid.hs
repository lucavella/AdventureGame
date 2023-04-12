{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.Grid (TileType(..), GridTile(..), GridRow, Grid, Coordinate(..), PlayerMove(..), makeMove, getTile, setTile, updateSeenGrid) where

    import Data.List (genericIndex)
    import Utils.List (mapWithIndex, setAtIndex)
    import System.Random (mkStdGen, randomR)


    -- different displayable types of tiles
    data TileType = Desert Bool | Water | Lava | Portal | Player | Undiscovered | OutOfBounds
        deriving Eq

    -- actual types of tiles
    data GridTile = GridTile {
        tileType :: TileType,
        seen :: Bool
    }

    type GridRow = [GridTile]
    type Grid = [GridRow]
    type Coordinate = (Integer, Integer)

    data PlayerMove = UpMove | RightMove | DownMove | LeftMove
        deriving (Eq, Enum)


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

    instance Show GridTile where
        show (GridTile t True) = show t
        show (GridTile _ False) = show Undiscovered

    makeMove :: Coordinate -> PlayerMove -> Maybe Coordinate
    makeMove (x, y) move = case move of
        DownMove -> Just (x, y + 1)
        RightMove -> Just (x + 1, y)
        UpMove -> if y > 0 then Just (x, y - 1) else Nothing
        LeftMove -> if x > 0 then Just (x - 1, y) else Nothing

    distance :: Coordinate -> Coordinate -> Double
    distance (x, y) (x', y') =
        sqrt $ fromInteger $ (x - x') ^ 2 + (y - y') ^ 2

    getTile :: Grid -> Coordinate -> GridTile
    getTile grid coord@(x, y)
        | x < 0 = GridTile OutOfBounds True
        | y < 0 = GridTile OutOfBounds True
        | otherwise = (uncurry . flip $ genericIndex . genericIndex grid) coord

    setTile :: Grid -> Coordinate -> GridTile -> Grid
    setTile grid (x, y) tile =
        setAtIndex y (setAtIndex x tile row) grid
        where
            row = genericIndex grid y
    
    updateSeenGrid :: Grid -> Coordinate -> Integer -> Grid
    updateSeenGrid grid position sight =
        mapWithIndex colFun grid
        where
            colFun y row = mapWithIndex (rowFun y) row
            rowFun y x (GridTile t seen)
                | distance (x, y) position <= fromInteger sight = GridTile t True
                | otherwise                                     = GridTile t seen