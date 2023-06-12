import Models.AdventureGame (AdventureGameConfig(..))
import Persistence.Parser
import Gui.GuiGame
import Gui.GuiAdventureGame
import Graphics.Gloss
import System.Random
import Control.Exception


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
        wormSpawnPct = 5
    }
    case result of
        Just () -> return ()
        Nothing -> putStrLn "An error ocurred" >> return ()

loadGameOpt :: IO ()
loadGameOpt = do
    ms <- loadGame saveFile
    case ms of
        Nothing -> putStrLn "Parse Error" >> main
        Just s -> runGame s

customGameOpt :: IO ()
customGameOpt = do
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

    putStr "Worm spawn rate % (default: 5.0) > "
    y <- getLineDefault "5"

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
            wormSpawnPct = read y
        } 
        case result of
            Just () -> return ()
            Nothing -> failed) `onException` failed

    where
        failed = putStrLn "Invalid game configuration" >> main



getLineDefault :: String -> IO String
getLineDefault defLine = do
    line <- getLine
    return $ if null line
             then defLine
             else line 