import Models.TerminalGame (runGame)
import Models.AdventureGame (AdventureGameConfig(..))

main :: IO ()
main = 
    -- runGame AdventureGameConfig {
    --     seed = 0,
    --     sight = 5,
    --     waterCap = 12,
    --     treasurePct = 50,
    --     waterPct = 5,
    --     portalPct = 1,
    --     lavaSinglePct = 10,
    --     lavaAdjacentPct = 50,
    --     gridDim = (30, 20)
    -- }
    -- runGame AdventureGameConfig {
    --     seed = 2,
    --     sight = 5,
    --     waterCap = 120,
    --     treasurePct = 50,
    --     waterPct = 1,
    --     portalPct = 0.01,
    --     lavaSinglePct = 30,
    --     lavaAdjacentPct = 40,
    --     gridDim = (30, 20)
    -- }
    runGame AdventureGameConfig {
        seed = 4380,
        sight = 5,
        waterCap = 120,
        treasurePct = 50,
        waterPct = 1,
        portalPct = 0.01,
        lavaSinglePct = 40,
        lavaAdjacentPct = 50,
        gridDim = (30, 20)
    }
