-- | This module defines the 'EventWriter' class.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.EventWriter.Class
  ( EventWriter (..), tellEvent
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Data.Semigroup (Semigroup)
import Control.Monad.Identity

import Reflex.Class (Event)


-- | 'EventWriter' efficiently collects 'Event' values using 'tellEvent'
-- and combines them via 'Semigroup' to provide an 'Event' result.
class (Monad m, Semigroup w) => EventWriter t (f :: * -> *) w m | m -> t w where
  tellEventF :: f (Event t w) -> m ()

tellEvent :: (EventWriter t Identity w m) => Event t w -> m ()
tellEvent = tellEventF . Identity


instance EventWriter t f w m => EventWriter t f w (ReaderT r m) where
  tellEventF = lift . tellEventF
