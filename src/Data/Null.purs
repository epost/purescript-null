module Data.Null where

import Prelude ((<$>), (<>), (||), not, class Show, class Functor, class Semigroup)
import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)

foreign import data Null :: * -> *

instance nullFunctor :: Functor Null where
  map = mapNull

instance nullSemigroup :: Semigroup a => Semigroup (Null a) where
  append x y = if isNull x || isNull y then null else pureNull (unsafeUnNull x <> unsafeUnNull y)

instance nullApply :: Apply Null where
  apply f x = if not (isNull f) then unsafeUnNull f <$> x else null

instance nullApplicative :: Applicative Null where
  pure = pureNull

instance nullBind :: Bind Null where
  bind x f = if isNull x then null else f (unsafeUnNull x)

foreign import null         :: forall a  .                       Null a
foreign import pureNull     :: forall a  .  a                 -> Null a
foreign import isNull       :: forall a  .             Null a -> Boolean
foreign import foldNull     :: forall a  .  a       -> Null a -> a
foreign import mapNull      :: forall a b. (a -> b) -> Null a -> Null b
foreign import showNull     :: forall a  .             Null a -> String

-- TODO hide
foreign import unsafeUnNull :: forall a  .             Null a -> a

instance nullShow :: Show (Null a) where
  show x = "(Null " <> showNull x <> ")"
