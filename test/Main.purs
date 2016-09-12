module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Null

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

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "n0 = " <> n0
  log $ show n1
  log $ "n2 = " <> n2
  log $ show $ map (1+_) n3
