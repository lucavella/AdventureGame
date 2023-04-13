import Models.AdventureGame
import Models.Grid
import Models.TerminalGame


getProfileSeed :: Int
getProfileSeed = loop 0
    where
        loop i = if goodEnough portalDistance
                 then i
                 else loop (i + 1)
            where
                Right state = initialState $ AdventureGameConfig {
                    seed = i,
                    sight = 5,
                    waterCap = 120,
                    treasurePct = 50,
                    waterPct = 1,
                    portalPct = 0.01,
                    lavaSinglePct = 40,
                    lavaAdjacentPct = 50,
                    gridDim = (30, 20)
                }
                portalDistance = closestDistance state [Water, Desert False, Desert True, Portal] [Portal]
                goodEnough Nothing = False
                goodEnough (Just n)
                    | n < 150 = False
                    | otherwise = True
