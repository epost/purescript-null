module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Null (Null, null, pureNull, foldNull)
import Data.Foldable (foldMap)
import Data.Traversable (sequence)

foreign import aNullInt :: Null Int
foreign import aNullString :: Null String

n0 :: String
n0 = foldNull "Alas, 't was nought!" aNullString

n1 :: Null String
n1 = map ("mapped over " <> _) aNullString

n2 :: String
n2 = foldNull "Alas once more!" n1

n3 :: Null Int
n3 = pureNull 41

n4 :: Null Int
n4 = aNullInt >>= (_ + 1) >>> pure

n5 :: Null Int
n5 = pure 3 >>= (_ + 1) >>> pure

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "n0 = " <> n0
  log $ show n1
  log $ "n2 = " <> n2
  log $ show $ map (1+_) n3
  log $ show $ n4
  log $ show $ n5

  -- Eq
  log $ show $ n1 == null
  log $ show $ null == (null :: Null Int)
  log $ show $ n3 == n3
  log $ show $ (pure 3 :: Null Int) /= (pure 4)

  -- Foldable
  log $ show $ foldMap id (pure "abc" :: Null String) == "abc"
  log $ show $ foldMap id (null :: Null String) == ""

  -- Traversable
  log $ show $ sequence (pure (Just "abc") :: Null (Maybe String)) == Just (pure "abc")
  log $ show $ sequence (null :: Null (Maybe String)) == Just null
