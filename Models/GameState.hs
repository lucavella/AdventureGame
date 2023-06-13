{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Models.GameState (
    GameState(..), GameConfig(..)
) where

    -- data class to represent the internal state of a game
    -- the game state corresponds to a data type corresponding to user commands
    class GameState s cmd | s -> cmd where
        nextState :: s -> cmd -> IO (Maybe s) -- state transition, given command
        isFinalState :: s -> Bool -- check if game is final

    -- data class to represent the internal configuration of a game
    -- the game config corresponds to a specific game state it generates
    class GameConfig c s | c -> s where
        initialState :: c -> Maybe s -- generate initial state from config, if config is valid
