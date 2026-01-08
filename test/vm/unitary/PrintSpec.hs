module PrintSpec (spec) where

import Test.Hspec
import Execution.State (newVMState, VMState(..))
import Types (Instruction(..), Value(..))
import Execution.Loop (execLoop)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
    describe "Print Instruction" $ do
        it "prints an integer from the stack" $ do
            let instrs = [PushInt 42, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "42\n"

        it "prints a string from the stack" $ do
            let instrs = [PushString "Hello", Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "Hello\n"

        it "prints a boolean from the stack" $ do
            let instrs = [PushBool True, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "#t\n"

        it "prints an error when the stack is empty" $ do
            let instrs = [Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldContain` "Error: Print requires a value on the stack"
