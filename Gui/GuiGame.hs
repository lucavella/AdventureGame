{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Gui.GuiGame (
    GuiGame(..),
    newGame, runGame
) where

    import Models.GameState
    import Control.Monad (unless)
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    
    class GameState s cmd => GuiGame s cmd where
        handleEvent :: Event -> s -> IO s
        renderMap :: s -> IO Picture


    newGame :: (GameConfig c s, GuiGame s cmd) => c -> IO (Maybe ())
    newGame c = sequence $ runGame <$> initialState c

    runGame :: GuiGame s cmd => s -> IO ()
    runGame s = playIO FullScreen black 0 s renderMap handleEvent (const return)