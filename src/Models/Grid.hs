{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.Grid (TileType(..), GridTile(..), GridRow, Grid, Coordinate(..), PlayerMove(..), makeMove, getTile, setTile, updateSeenGrid) where

    import Data.List (genericIndex)
    import Utils.List (mapWithIndex, setAtIndex)
    import System.Random (mkStdGen, randomR)


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
    type Grid = [GridRow]

    type Coordinate = (Integer, Integer)

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

    -- Euclidean distance between 2 coordinates
    distance :: Coordinate -> Coordinate -> Double
    distance (x, y) (x', y') =
        sqrt $ fromInteger $ (x - x') ^ 2 + (y - y') ^ 2

    -- gets a grid tile based on a coordinate
    getTile :: Grid -> Coordinate -> GridTile
    getTile grid coord@(x, y)
        | x < 0 = GridTile OutOfBounds True
        | y < 0 = GridTile OutOfBounds True
        | otherwise = (uncurry . flip $ genericIndex . genericIndex grid) coord

    -- returns a new grid by changing a grid tile on the desired coordinate
    setTile :: Grid -> Coordinate -> GridTile -> Grid
    setTile grid (x, y) tile =
        setAtIndex y (setAtIndex x tile row) grid
        where
            row = genericIndex grid y
    
    -- updates the grid by marking all grid tiles as seen in a radius around a given coordinate
    -- uses lazy evaluation to iterate over the entire infinite grid
    updateSeenGrid :: Grid -> Coordinate -> Integer -> Grid
    updateSeenGrid grid position sight =
        mapWithIndex colFun grid
        where
            colFun y row = mapWithIndex (rowFun y) row
            rowFun y x (GridTile t seen)
                | distance (x, y) position <= fromInteger sight = GridTile t True
                | otherwise                                     = GridTile t seen