module ListOperationsSpec (spec) where

import Test.Hspec
import Execution.State (newVMState, VMState(..))
import Types (Instruction(..), Value(..))
import Execution.Loop (execLoop)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
    describe "List Operations" $ do
        describe "IsEmpty" $ do
            it "Empty list returns True" $ do
                let instrs = [ListMake 0, IsEmpty, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Non-empty list returns False" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, IsEmpty, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

        describe "Nth" $ do
            it "Access first element (index 0)" $ do
                let instrs = [PushInt 10, PushInt 20, PushInt 30, ListMake 3, PushInt 0, Nth, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "10\n"

            it "Access middle element (index 1)" $ do
                let instrs = [PushInt 10, PushInt 20, PushInt 30, ListMake 3, PushInt 1, Nth, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "20\n"

            it "Access last element (index 2)" $ do
                let instrs = [PushInt 10, PushInt 20, PushInt 30, ListMake 3, PushInt 2, Nth, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "30\n"

            it "Out of bounds index fails" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, PushInt 5, Nth, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldContain` "Error"

            it "Negative index fails" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, PushInt (-1), Nth, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldContain` "Error"

        describe "Insert" $ do
            it "Insert at beginning (index 0)" $ do
                let instrs = [PushInt 1, PushInt 2, PushInt 3, ListMake 3, PushInt 0, PushInt 99, Insert, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(99 1 2 3)\n"

            it "Insert at middle (index 1)" $ do
                let instrs = [PushInt 1, PushInt 2, PushInt 3, ListMake 3, PushInt 1, PushInt 99, Insert, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 99 2 3)\n"

            it "Insert at end (index = length)" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, PushInt 2, PushInt 99, Insert, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 2 99)\n"

            it "Insert in empty list at index 0" $ do
                let instrs = [ListMake 0, PushInt 0, PushInt 42, Insert, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(42)\n"

            it "Out of bounds index fails" $ do
                let instrs = [PushInt 1, ListMake 1, PushInt 5, PushInt 99, Insert, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldContain` "Error"

        describe "Remove" $ do
            it "Remove first element (index 0)" $ do
                let instrs = [PushInt 10, PushInt 20, PushInt 30, ListMake 3, PushInt 0, Remove, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(20 30)\n"

            it "Remove middle element (index 1)" $ do
                let instrs = [PushInt 10, PushInt 20, PushInt 30, ListMake 3, PushInt 1, Remove, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(10 30)\n"

            it "Remove last element (index 2)" $ do
                let instrs = [PushInt 10, PushInt 20, PushInt 30, ListMake 3, PushInt 2, Remove, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(10 20)\n"

            it "Remove from single element list" $ do
                let instrs = [PushInt 42, ListMake 1, PushInt 0, Remove, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "()\n"

            it "Out of bounds index fails" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, PushInt 5, Remove, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldContain` "Error"

        describe "Contains" $ do
            it "Element is present (Int)" $ do
                let instrs = [PushInt 1, PushInt 2, PushInt 3, ListMake 3, PushInt 2, Contains, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Element is not present (Int)" $ do
                let instrs = [PushInt 1, PushInt 2, PushInt 3, ListMake 3, PushInt 99, Contains, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "Element is present (Bool)" $ do
                let instrs = [PushBool True, PushBool False, ListMake 2, PushBool True, Contains, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Empty list contains nothing" $ do
                let instrs = [ListMake 0, PushInt 42, Contains, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#f\n"

            it "Contains with Float" $ do
                let instrs = [PushFloat 1.5, PushFloat 2.5, PushFloat 3.5, ListMake 3, PushFloat 2.5, Contains, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

        describe "Append" $ do
            it "Append two non-empty lists" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, PushInt 3, PushInt 4, ListMake 2, Append, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 2 3 4)\n"

            it "Append empty list to non-empty list" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, ListMake 0, Append, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 2)\n"

            it "Append non-empty list to empty list" $ do
                let instrs = [ListMake 0, PushInt 1, PushInt 2, ListMake 2, Append, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 2)\n"

            it "Append two empty lists" $ do
                let instrs = [ListMake 0, ListMake 0, Append, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "()\n"

            it "Append three lists sequentially" $ do
                let instrs = [PushInt 1, ListMake 1, PushInt 2, ListMake 1, Append, PushInt 3, ListMake 1, Append, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 2 3)\n"

        describe "Reverse" $ do
            it "Reverse non-empty list" $ do
                let instrs = [PushInt 1, PushInt 2, PushInt 3, PushInt 4, ListMake 4, Reverse, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(4 3 2 1)\n"

            it "Reverse empty list" $ do
                let instrs = [ListMake 0, Reverse, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "()\n"

            it "Reverse single element list" $ do
                let instrs = [PushInt 42, ListMake 1, Reverse, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(42)\n"

            it "Double reverse returns original" $ do
                let instrs = [PushInt 1, PushInt 2, PushInt 3, ListMake 3, Reverse, Reverse, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 2 3)\n"

            it "Reverse with mixed types" $ do
                let instrs = [PushInt 1, PushBool True, PushFloat 3.5, ListMake 3, Reverse, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(3.5 #t 1)\n"

        describe "Complex List Operations" $ do
            it "Combine Insert and Remove" $ do
                let instrs = [PushInt 1, PushInt 2, PushInt 3, ListMake 3, PushInt 1, PushInt 99, Insert, PushInt 2, Remove, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(1 99 3)\n"

            it "Append then Reverse" $ do
                let instrs = [PushInt 1, PushInt 2, ListMake 2, PushInt 3, PushInt 4, ListMake 2, Append, Reverse, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "(4 3 2 1)\n"

            it "IsEmpty after Remove all elements" $ do
                let instrs = [PushInt 42, ListMake 1, PushInt 0, Remove, IsEmpty, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "#t\n"

            it "Nth after Insert" $ do
                let instrs = [PushInt 10, PushInt 20, ListMake 2, PushInt 1, PushInt 15, Insert, PushInt 1, Nth, Print, Halt]
                let state = newVMState instrs [] [] []
                output <- capture_ (execLoop state)
                output `shouldBe` "15\n"
