{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Models.TerminalGame (Command, GameState(..), TerminalGame(..), runGame) where

    import Data.Char (isAlphaNum)
    import Control.Monad (unless)


    -- user input for a simple terminal-based game is just a single-line string
    type Command = String

    promptForInput :: IO Command
    promptForInput = putStr "> " >> fmap (filter isAlphaNum) getLine

    -- We use a type s to represent a game state, where ...
    -- ... nextState computes the next game state, given the current state and next user input (may fail on invalid input)
    -- ... isFinalState checks whether a given state is a final state 
    class GameState s where
        nextState :: s -> Command -> Maybe s
        isFinalState :: s -> Bool

    -- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
    -- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
    class GameState s => TerminalGame s c | c -> s where
        initialState :: c -> Either String s

    -- run a game in the terminal
    runGame :: (Show s, TerminalGame s c) => c -> IO ()
    runGame = either error loop . initialState
        where loop st = do print st
                           unless (isFinalState st) $ do
                                cmd <- promptForInput
                                let nxt = nextState st cmd
                                maybe (putStrLn "Invalid input, please try again" >> loop st) loop nxt