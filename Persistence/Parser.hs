{-# LANGUAGE RecordWildCards #-}

module Persistence.Parser (loadGame) where

    import Models.Grid
    import Models.AdventureGame
    import qualified Data.Map as M
    import qualified Data.Set as S
    import Text.Parsec
    import System.Directory


    data ParsedVal = Natural Int
                   | FloatP Double
                   | Position Coordinate
                   | PositionSet (S.Set Coordinate)
                   | PositionsList [[Coordinate]]

    -- data type to incrementally store parsed values
    type AdventureGameStateParsed = M.Map String ParsedVal


    -- initializes list fields of parsed values state
    initialParsedState :: AdventureGameStateParsed
    initialParsedState = M.fromList [
        ("revealed", PositionSet S.empty), ("collected", PositionSet S.empty),
        ("emerging", PositionsList []), ("disappearing", PositionsList []) ]

    -- attempts to restore game state from parsed values state
    parsedToState :: AdventureGameStateParsed -> Maybe AdventureGameState
    parsedToState parsed = do
        Position pos <- M.lookup "position" parsed
        Natural sup <- M.lookup "supply" parsed
        PositionSet rev <- M.lookup "revealed" parsed
        PositionSet col <- M.lookup "collected" parsed
        PositionsList em <- M.lookup "emerging" parsed
        PositionsList dis <- M.lookup "disappearing" parsed
        Natural s <- M.lookup "s" parsed
        Natural m <- M.lookup "m" parsed
        Natural g <- M.lookup "g" parsed
        FloatP t <- M.lookup "t" parsed
        FloatP w <- M.lookup "w" parsed
        FloatP p <- M.lookup "p" parsed
        FloatP l <- M.lookup "l" parsed
        FloatP ll <- M.lookup "ll" parsed
        Natural x <- M.lookup "x" parsed
        FloatP y <- M.lookup "y" parsed

        let gc = AdventureGameConfig { -- restore game config
            seed = g,
            sight = s,
            waterCap = m,
            treasurePct = t,
            waterPct = w,
            portalPct = p,
            lavaSinglePct = l,
            lavaAdjacentPct = ll,
            wormLength = x,
            wormSpawnPct = y,
            bmpTiles = M.empty
        }
        grid <- restoreGrid (initGrid gc) (toInteger s) rev col -- recreate grid
        grid' <- makeMoveGrid grid pos -- set grid position
        let Grid _ tile _ = grid'

        return AdventureGameState { -- restore game state
            grid = grid',
            event = if (tileType tile == Water) then ReplenishedWater else NoEvent,
            water = sup,
            tilesVisited = rev,
            treasureCollected = col,
            wormsEmerging = em,
            wormsDisappearing = dis,
            gameConfig = gc,
            message = Nothing
        }

    -- recreates grid from seed, updates collected desert treasure tiles and seen tiles
    -- revealed tiles in config represent visited tiles, from which revealed tiles can be infered (with sight config parameter)
    restoreGrid :: Grid -> Integer -> S.Set Coordinate -> S.Set Coordinate -> Maybe Grid
    restoreGrid grid sight rev col = do
        let grid' = foldr (\c g -> setTileAt g c $ GridTile { tileType = DesertEmpty, seen = True }) grid col
        foldr (\c g -> g >>= flip makeMoveGrid c >>= \g' -> return $ updateSeenGrid sight g') (Just grid') rev


    type Parser = Parsec String AdventureGameStateParsed


    natural :: (Read n, Integral n) => Parser n
    natural = read <$> many1 digit

    negative :: (Read n, Integral n) => Parser n
    negative = char '-' >> (0 -) <$> natural

    integer :: (Read n, Integral n) => Parser n
    integer = negative <|> natural

    float :: (Read n, Floating n) => Parser n
    float = do
        w <- fmap show integer
        d <- option "" $ (:) <$> char '.' <*> fmap show natural
        e <- option "" $ (:) <$> oneOf "eE" <*> fmap show integer
        return . read $ w ++ d ++ e


    -- parses with space padding
    spacePadded :: Parser a -> Parser a
    spacePadded p = do
        spaces
        val <- p
        spaces
        return val

    -- parses given keyword by trying it
    keyword :: String -> Parser String
    keyword = try . spacePadded . string

    -- parses enclosed by parenthesis
    parens :: Parser a -> Parser a
    parens p = do
        spacePadded $ char '('
        val <- p
        spacePadded $ char ')'
        return val

    -- parses enclosed by brackets
    brackets :: Parser a -> Parser a
    brackets p = do
        spacePadded $ char '['
        val <- p
        spacePadded $ char ']'
        return val


    -- parses a coordinate
    positionParse :: Parser Coordinate
    positionParse = brackets $ do
        x <- natural
        spacePadded $ char ','
        y <- natural
        return (x, y)

    -- parses a list of positions
    -- implements POSITIONS -> POSITION , POSITIONS instead of POSITIONS -> , POSITION POSITIONS
    positionsParse :: Parser [Coordinate]
    positionsParse = sepBy1 positionParse . spacePadded $ char ','

    -- parses a line of the save file
    lineParse :: String -> Parser a -> Parser a
    lineParse s p = keyword s >> parens p

    -- parses all lines of the save file
    -- implements LINES -> LINE \n LINES instead of LINES -> \n LINE LINES
    linesParse :: Parser AdventureGameStateParsed
    linesParse = many1 line >> eof >> getState
        where
            line = do
                s <- getState
                s' <- choice [
                    lpSingle "position" Position positionParse s,
                    lpSingle "supply" Natural natural s,
                    lpSet "revealed" positionParse s,
                    lpSet "collected" positionParse s,
                    lpList "emerging" positionsParse s,
                    lpList "disappearing" positionsParse s,
                    lpSingle "s" Natural natural s,
                    lpSingle "m" Natural natural s,
                    lpSingle "g" Natural integer s, -- seed is a signed integer
                    lpSingle "t" FloatP float s,
                    lpSingle "w" FloatP float s,
                    lpSingle "p" FloatP float s,
                    lpSingle "ll" FloatP float s,
                    lpSingle "l" FloatP float s,
                    lpSingle "x" Natural natural s,
                    lpSingle "y" FloatP float s ]
                putState s'
            
            lpSingle kw f p s = lineParse kw p >>= \v -> return $ M.insert kw (f v) s -- uses parser for single value and stores it in parsed values state
            lpSet kw p s = lineParse kw p >>= \v -> return $ M.adjust (posSet $ S.insert v) kw s -- uses parser for value stored in set
            lpList kw p s = lineParse kw p >>= \v -> return $ M.adjust (posList (v:)) kw s -- uses parser for value stored in list
            posSet f (PositionSet ps) = PositionSet $ f ps
            posList f (PositionsList pss) = PositionsList $ f pss


    -- runs the parser on the save file and returns the game state if successful
    -- implements GAME -> LINES instead of GAME -> LINE LINES
    gameStateParser :: String -> Maybe AdventureGameState
    gameStateParser input = do
        case runParser linesParse initialParsedState "Loading save file" input of
            Left _ -> Nothing
            Right parsed -> parsedToState parsed


    -- loads the game by first checking if the save file exists and then parsing it
    loadGame :: String -> IO (Maybe AdventureGameState)
    loadGame fname = do 
        exists <- doesFileExist fname
        if exists
        then gameStateParser <$> readFile fname
        else return Nothing