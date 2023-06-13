import Models.AdventureGame (AdventureGameConfig(..))
import Persistence.Parser
import GUI.GUIGame
import GUI.GUIAdventureGame
import GUI.GUITiles
import qualified Data.Map as M
import Graphics.Gloss
import System.Random
import Control.Exception


-- program entry point with simple menu
main :: IO ()
main = do
    putStrLn "Choose an option"
    putStrLn "(1) New game"
    putStrLn "(2) Load game"
    putStrLn "(3) New custom game"
    putStrLn "(0) Exit"
    putStr "Option (default: 1) > "
    gm <- getLineDefault "1"
    case gm of
        "1" -> newGameOpt
        "2" -> loadGameOpt
        "3" -> customGameOpt
        "0" -> return ()
        otherwise -> putStrLn "Invalid input" >> main


-- first option: start new game with default settings
newGameOpt :: IO ()
newGameOpt = do
    g <- randomIO
    result <- newGame AdventureGameConfig {
        seed = g,
        sight = 10,
        waterCap = 15,
        treasurePct = 10,
        waterPct = 5,
        portalPct = 0.5,
        lavaSinglePct = 5,
        lavaAdjacentPct = 50,
        wormLength = 10,
        wormSpawnPct = 15,
        bmpTiles = M.empty
    }
    case result of
        Just () -> return ()
        Nothing -> putStrLn "An error ocurred" >> return () -- cannot happen with hard-coded config

-- second option: start saved game
loadGameOpt :: IO ()
loadGameOpt = do
    ms <- loadGame saveFile
    case ms of
        Nothing -> putStrLn "Parse Error" >> main -- back to menu if loading fails
        Just s -> runGame s

-- third option: start new game with custom settings
customGameOpt :: IO ()
customGameOpt = do
    -- gloss increasingly slows down when using bitmaps to render grid
    -- this might be due to a memory leak, so bitmap graphics are not recommended
    putStr "Bitmap graphics y/n (potentially bad performance, default n) > "
    b <- getLineDefault "n"

    putStr "Seed (default: random) > "
    g' <- randomIO
    g <- getLineDefault $ show (g' :: Int)

    putStr "Sight (default: 10) > "
    s <- getLineDefault "10"

    putStr "Water capacity (default: 15) > "
    m <- getLineDefault "15"

    putStr "Treasure likelihood % (default: 10.0) > "
    t <- getLineDefault "10"

    putStr "Water likelihood % (default: 5.0) > "
    w <- getLineDefault "5"

    putStr "Portal likelihood % (default: 0.5) > "
    p <- getLineDefault "0.5"

    putStr "Lava likelihood % (default: 5.0) > "
    l <- getLineDefault "5"

    putStr "Adjacent lava likelihood % (default: 50.0) > "
    ll <- getLineDefault "50"

    putStr "Worms length (default: 10) > "
    x <- getLineDefault "10"

    putStr "Worm spawn rate % (default: 15.0) > "
    y <- getLineDefault "15"
    
    bmps <- loadBMPTiles

    -- try creating game config
    (do
        result <- newGame AdventureGameConfig {
            seed = read g,
            sight = read s,
            waterCap = read m,
            treasurePct = read t,
            waterPct = read w,
            portalPct = read p,
            lavaSinglePct = read l,
            lavaAdjacentPct = read ll,
            wormLength = read x,
            wormSpawnPct = read y,
            bmpTiles = if b == "y" then bmps else M.empty
        } 
        case result of
            Just () -> return ()
            Nothing -> failed) `onException` failed -- exception occurs on invalid value for read

    where
        failed = putStrLn "Invalid game configuration" >> main


-- get input from stdin with default value if nothing provided
getLineDefault :: String -> IO String
getLineDefault defLine = do
    line <- getLine
    return $ if null line
             then defLine
             else line 