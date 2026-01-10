module FileManagementSpec (spec) where

import Test.Hspec
import Execution.Ops.FileManagement (opOpenFile, opWriteFile, opCloseFile)
import Execution.State (VMState(..), newVMState)
import Types (Value(..))
import System.IO (IOMode(..))
import Control.Exception (evaluate)

-- Note: Real IO testing is harder in unit tests without mocking.
-- Here we test the structural behavior on the stack mainly, 
-- or integration tests will cover the actual file creation.

spec :: Spec
spec = do
    describe "OpOpenFile" $ do
        it "returns error if stack is failing arguments" $ do
            let state = (newVMState [] [] []) { stack = [] }
            result <- opOpenFile state
            result `shouldSatisfy` isLeft

    describe "OpCloseFile" $ do
        it "removes FileVal from stack" $ do
            let state = (newVMState [] [] []) { stack = [FileVal "test.txt"] }
            result <- opCloseFile state
            case result of
                Right newState -> stack newState `shouldBe` []
                Left err -> expectationFailure $ "Expected success, got: " ++ err

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
