module Auxiliary.State where

import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.State
import Game.GameState

-- ~ Auxiliary function that morphs from State to StateT.
morphStateFunction :: State GameState a -> StateT GameState IO a
morphStateFunction stateFunction = hoist (return . runIdentity) stateFunction
