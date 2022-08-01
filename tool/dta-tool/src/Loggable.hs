module Loggable(
Loggable(..)
)
where

import Language.C.Analysis.TravMonad
import Language.C.Pretty

class Monad m => Loggable m where
    log :: (Show a) => a -> m ()
    logPretty :: (Pretty a) => a -> m ()
