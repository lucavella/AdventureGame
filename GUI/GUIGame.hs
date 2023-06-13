{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.GUIGame (
    GUIGame(..),
    newGame, runGame
) where
    
    import Models.GameState
    import Graphics.Gloss.Interface.IO.Game
    

    -- data class for GUI gloss games
    class GameState s cmd => GUIGame s cmd where
        handleEvent :: Event -> s -> IO s -- update state given user input
        renderState :: s -> IO Picture  -- render state as picture


    -- runs new GUI game from game config
    newGame :: (GameConfig c s, GUIGame s cmd) => c -> IO (Maybe ())
    newGame c = sequence $ runGame <$> initialState c

    -- runs new GUI game from game state
    runGame :: GUIGame s cmd => s -> IO ()
    runGame s = playIO FullScreen black 0 s renderState handleEvent (const return)