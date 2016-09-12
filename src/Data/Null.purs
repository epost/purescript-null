module Data.Null where

import Prelude ((<>), class Show, class Functor)

foreign import data Null :: * -> *

instance nullFunctor :: Functor Null where
  map = mapNull
    
foreign import pureNull :: forall a  .  a                 -> Null a
foreign import isNull   :: forall a  .             Null a -> Boolean
foreign import foldNull :: forall a  .  a       -> Null a -> a
foreign import mapNull  :: forall a b. (a -> b) -> Null a -> Null b
foreign import showNull :: forall a  .             Null a -> String

instance nullShow :: Show (Null a) where
  show x = "(Null " <> showNull x <> ")"
