module Util where

import qualified Data.Sequence as S

-- Queue definition
type Queue a = S.Seq a

emptyQueue :: Queue a
emptyQueue = S.empty

queueIsEmpty :: Queue a -> Bool
queueIsEmpty = S.null

queuePush :: Queue a -> a -> Queue a
queuePush = (S.|>)

queuePop :: Queue a -> Queue a
queuePop xs = snd(S.splitAt 1 xs)

queueFront :: Queue a -> a
queueFront xs = S.index xs 0
