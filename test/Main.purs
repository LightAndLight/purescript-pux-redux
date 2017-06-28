module Test.Main where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Symbol (SProxy(..))
import Pux.Redux

import Control.Monad.Eff (Eff)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let x = addField (SProxy :: SProxy "two") 2 {}
  logShow x.two

  let y = addField (SProxy :: SProxy "one") 1 x
  logShow $ y.one
  logShow $ y.two

  let z = removeField (SProxy :: SProxy "two") y
  logShow $ z.one
