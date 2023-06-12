{-# LANGUAGE RecordWildCards #-}

module Persistence.Parser (loadGame) where

    import Models.Grid
    import Models.AdventureGame
    import qualified Data.Map.Strict as M
    import qualified Data.Set as S
    import Text.Parsec hiding (token)
    import Text.Parsec.Char
    import System.Directory


    data ParsedVal = Natural Int
                   | FloatP Double
                   | Position Coordinate
                   | Positions (S.Set Coordinate)
                   | PositionsList (S.Set [Coordinate])

    type AdventureGameStateParsed = M.Map String ParsedVal

    initialParsedState :: AdventureGameStateParsed
    initialParsedState = M.fromList [
        ("revealed", Positions S.empty), ("collected", Positions S.empty),
        ("emerging", PositionsList S.empty), ("disappearing", PositionsList S.empty) ]

    parsedToState :: AdventureGameStateParsed -> Maybe AdventureGameState
    parsedToState parsed = do
        Position pos <- M.lookup "position" parsed
        Natural sup <- M.lookup "supply" parsed
        Positions rev <- M.lookup "revealed" parsed
        Positions col <- M.lookup "collected" parsed
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

        let gc = AdventureGameConfig {
            seed = g,
            sight = s,
            waterCap = m,
            treasurePct = t,
            waterPct = w,
            portalPct = p,
            lavaSinglePct = l,
            lavaAdjacentPct = ll,
            wormLength = x,
            wormSpawnPct = y
        }
        grid <- restoreGrid (initGrid gc) (toInteger s) rev col
        grid' <- makeMoveGrid grid pos
        let Grid _ tile _ = grid'

        return AdventureGameState {
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

    restoreGrid :: Grid -> Integer -> S.Set Coordinate -> S.Set Coordinate -> Maybe Grid
    restoreGrid grid sight rev col = do
        let grid' = foldr (\c g -> setTileAt g c $ GridTile { tileType = Desert False, seen = True }) grid col
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


    spacePadded :: Parser a -> Parser a
    spacePadded p = do
        spaces
        val <- p
        spaces
        return val

    token :: Parser a -> Parser a
    token p = try $ spacePadded p

    keyword :: String -> Parser String
    keyword = token . string

    parens :: Parser a -> Parser a
    parens p = do
        spacePadded $ char '('
        val <- p
        spacePadded $ char ')'
        return val

    brackets :: Parser a -> Parser a
    brackets p = do
        spacePadded $ char '['
        val <- p
        spacePadded $ char ']'
        return val


    positionParse :: Parser Coordinate
    positionParse = brackets $ do
        x <- natural
        spacePadded $ char ','
        y <- natural
        return (x, y)

    -- implements POSITIONS -> POSITION , POSITIONS instead of POSITIONS -> , POSITION POSITIONS
    positionsParse :: Parser [Coordinate]
    positionsParse = sepBy1 positionParse . spacePadded $ char ','

    lineParse :: String -> Parser a -> Parser a
    lineParse s p = keyword s >> parens p

    -- implements LINES -> LINE \n LINES instead of LINES -> \n LINE LINES
    linesParse :: Parser AdventureGameStateParsed
    linesParse = many1 line >> eof >> getState
        where
            line = do
                s <- getState
                s' <- choice [
                    lpSingle "position" Position positionParse s,
                    lpSingle "supply" Natural natural s,
                    lpList "revealed" positionsSet positionParse s,
                    lpList "collected" positionsSet positionParse s,
                    lpList "emerging" positionsListSet positionsParse s,
                    lpList "disappearing" positionsListSet positionsParse s,
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
            
            lpSingle kw f p s = lineParse kw p >>= \v -> return $ M.insert kw (f v) s
            lpList kw f p s = lineParse kw p >>= \v -> return $ M.adjust (f $ S.insert v) kw s
            positionsSet f (Positions ps) = Positions $ f ps
            positionsListSet f (PositionsList pss) = PositionsList $ f pss


    -- implements GAME -> LINES instead of GAME -> LINE LINES
    gameStateParser :: String -> Maybe AdventureGameState
    gameStateParser input = do
        case runParser linesParse initialParsedState "Loading save file" input of
            Left _ -> Nothing
            Right parsed -> parsedToState parsed


    loadGame :: String -> IO (Maybe AdventureGameState)
    loadGame fname = do 
        exists <- doesFileExist fname
        if exists
        then gameStateParser <$> readFile fname
        else return Nothing