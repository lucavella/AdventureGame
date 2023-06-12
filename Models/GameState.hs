{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Models.GameState (
    GameState(..), GameConfig(..)
) where

    -- We use a type s to represent a game state, where ...
    -- ... nextState computes the next game state, given the current state and next user input (may fail on invalid input)
    -- ... isFinalState checks whether a given state is a final state
    -- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
    -- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
    class GameState s cmd | s -> cmd where
        nextState :: s -> cmd -> Maybe s
        isFinalState :: s -> Bool


    class GameConfig c s | c -> s where
        initialState :: c -> Maybe s
