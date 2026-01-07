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

    describe "Logical Operations" $ do
        describe "And" $ do
            it "True && True = True" $ do
                let instrs = [PushBool True, PushBool True, And, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "True && False = False" $ do
                let instrs = [PushBool True, PushBool False, And, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "False && True = False" $ do
                let instrs = [PushBool False, PushBool True, And, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "False && False = False" $ do
                let instrs = [PushBool False, PushBool False, And, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "And with comparison results" $ do
                let instrs = [PushInt 5, PushInt 3, Lt, PushInt 10, PushInt 10, Eq, And, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

        describe "Or" $ do
            it "True || True = True" $ do
                let instrs = [PushBool True, PushBool True, Or, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "True || False = True" $ do
                let instrs = [PushBool True, PushBool False, Or, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "False || True = True" $ do
                let instrs = [PushBool False, PushBool True, Or, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "False || False = False" $ do
                let instrs = [PushBool False, PushBool False, Or, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "Or with comparison results" $ do
                let instrs = [PushInt 5, PushInt 10, Gt, PushInt 20, PushInt 20, Eq, Or, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

        describe "Not" $ do
            it "Not True = False" $ do
                let instrs = [PushBool True, Not, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "Not False = True" $ do
                let instrs = [PushBool False, Not, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Not with comparison result" $ do
                let instrs = [PushInt 7, PushInt 7, Eq, Not, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "Double negation" $ do
                let instrs = [PushBool True, Not, Not, Print, Halt]
                let state = newVMState instrs [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"
