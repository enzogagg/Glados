module ComparisonSpec (spec) where

import Test.Hspec
import Execution.State (newVMState, VMState(..))
import Types (Instruction(..), Value(..))
import Execution.Loop (execLoop)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
    describe "Numeric Comparisons" $ do
        describe "Equality (Eq)" $ do
            it "Int == Int" $ do
                let instrs = [PushInt 42, PushInt 42, Eq, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Float == Float" $ do
                let instrs = [PushFloat 42.5, PushFloat 42.5, Eq, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Int == Float (Mixed)" $ do
                let instrs = [PushInt 42, PushFloat 42.0, Eq, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Float == Int (Mixed)" $ do
                let instrs = [PushFloat 42.0, PushInt 42, Eq, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

        describe "Inequality (Neq)" $ do
            it "Int != Float (True)" $ do
                let instrs = [PushInt 42, PushFloat 42.1, Neq, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

        describe "Less Than (Lt)" $ do
            it "Int < Float (True)" $ do
                let instrs = [PushInt 42, PushFloat 42.5, Lt, Print, Halt] -- 42 < 42.5
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Float < Int (True)" $ do
                let instrs = [PushFloat 42.5, PushInt 43, Lt, Print, Halt] -- 42.5 < 43
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

        describe "Greater Than (Gt)" $ do
            it "Float > Int (True)" $ do
                let instrs = [PushFloat 42.5, PushInt 42, Gt, Print, Halt] -- 42.5 > 42
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"
