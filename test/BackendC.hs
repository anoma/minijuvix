module BackendC where

import BackendC.Positive qualified as P
import BackendC.Examples qualified as E
import Base

allTests :: TestTree
allTests = testGroup "Backend C tests" [P.allTests, E.allTests]
