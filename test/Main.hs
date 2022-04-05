module Main (main) where

import Base

import qualified TypeCheck
import qualified Scope

allTests :: TestTree
allTests = testGroup "MiniJuvix tests" $
  [ Scope.allTests,
    TypeCheck.allTests
  ]

main :: IO ()
main = defaultMain allTests
