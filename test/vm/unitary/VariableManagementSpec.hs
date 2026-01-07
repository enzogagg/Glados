module VariableManagementSpec (spec) where

import Test.Hspec
import Execution.Ops.VariableManagement
import Execution.State (VMState(..), newVMState)
import Types (Value(..))
import qualified Data.Map as Map

spec :: Spec
spec = describe "Variable Management" $ do

    describe "opDefine" $ do
        it "defines a new variable in the current environment" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 42] }
            let name = "x"
            case opDefine name state of
                Right newState -> do
                    stack newState `shouldBe` []
                    Map.lookup name (env newState) `shouldBe` Just (IntVal 42)
                Left err -> expectationFailure err

        it "fails if stack is empty" $ do
            let state = (newVMState [] [] []) { stack = [] }
            opDefine "x" state `shouldSatisfy` isLeft

    describe "opLoad" $ do
        it "loads an existing variable onto the stack" $ do
            let state = (newVMState [] [] []) { env = Map.fromList [("x", IntVal 10)] }
            opLoad "x" state `shouldBe` Right (state { stack = [IntVal 10] })

        it "fails if variable is undefined" $ do
            let state = (newVMState [] [] []) { env = Map.fromList [] }
            opLoad "z" state `shouldSatisfy` isLeft

    describe "opStore" $ do
        it "updates an existing variable" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 99], env = Map.fromList [("x", IntVal 10)] }
            case opStore "x" state of
                 Right newState -> Map.lookup "x" (env newState) `shouldBe` Just (IntVal 99)
                 Left err -> expectationFailure err

        it "fails if variable is undefined (cannot store to unknown)" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 99], env = Map.fromList [] }
            opStore "z" state `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
