{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Gui.GuiGame (
    GuiGame(..),
    runGame
) where

    import Models.GameState
    import Control.Monad (unless)
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    
    class GameState s cmd => GuiGame s cmd where
        handleEvent :: Event -> s -> IO s
        renderMap :: s -> IO Picture


    runGame :: (GameConfig c s, GuiGame s cmd) => c -> IO ()
    runGame = either error startGame . initialState
        where
            nextFrame _ = return
            startGame s = playIO FullScreen black 10 s renderMap handleEvent nextFrame