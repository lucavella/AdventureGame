import Models.TerminalGame (runGame)
import Models.AdventureGame (AdventureGameConfig(..))

main :: IO ()
main = 
    runGame AdventureGameConfig {
        seed = 2,
        sight = 5,
        waterCap = 12,
        treasurePct = 50,
        waterPct = 5,
        portalPct = 0.1,
        lavaSinglePct = 5,
        lavaAdjacentPct = 50,
        gridDim = (30, 20)
    }