module TypeCheck (allTests) where

import Base
import qualified TypeCheck.Negative as N
import qualified TypeCheck.Positive as P

allTests :: TestTree
allTests = testGroup "TypeCheck tests" [P.allTests, N.allTests]
