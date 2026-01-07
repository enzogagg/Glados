module ExecutionLoopSpec (spec) where

import Test.Hspec
import System.IO.Silently (capture_)
import Execution.Loop (execLoop)
import Execution.State (VMState(..), newVMState)
import Types

spec :: Spec
spec = describe "ExecutionLoop" $ do
    it "executes a simple Print loop" $ do
        let instrs = [PushInt 42, Print, Halt]
        let state = (newVMState instrs [] [])
        output <- capture_ (execLoop state)
        output `shouldContain` "42"

    it "handles Halt" $ do
        let instrs = [Halt]
        let state = (newVMState instrs [] [])
        output <- capture_ (execLoop state)
        output `shouldBe` ""

    it "reports error on division by zero" $ do
        let instrs = [PushInt 1, PushInt 0, Div, Halt]
        let state = (newVMState instrs [] [])
        output <- capture_ (execLoop state)
        output `shouldContain` "Division by zero"
    
    it "reports error on invalid instruction pointer" $ do
        let instrs = []
        let state = (newVMState instrs [] [])
        -- IP 0, Length 0. 0 >= 0 is True. Out of bounds.
        output <- capture_ (execLoop state)
        output `shouldContain` "Instruction pointer out of bounds"
