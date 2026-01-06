module ComplexDataStructuresSpec (spec) where

import Test.Hspec
import Execution.State (newVMState, VMState(..))
import Types (Instruction(..), Value(..))
import Execution.Loop (execLoop)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
    describe "Tuple Operations" $ do
        it "Tuples creation" $ do
            let instrs = [PushInt 1, PushInt 2, MakeTuple 2, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "{1 2}\n"
        
        it "Gets element from Tuple" $ do
            let instrs = [PushInt 1, PushInt 2, MakeTuple 2, TupleGet 0, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "1\n"

    describe "Array Operations" $ do
        it "Creates Array" $ do
            let instrs = [PushInt 1, PushInt 2, MakeArray 2, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "[1 2]\n"

        it "Gets element from Array" $ do
            let instrs = [PushInt 1, PushInt 2, MakeArray 2, PushInt 0, ArrayGet, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "1\n"

        it "Sets element in Array" $ do
            let instrs = [PushInt 1, PushInt 2, MakeArray 2, PushInt 0, PushInt 99, ArraySet, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "[99 2]\n"

    describe "Map Operations" $ do
        it "Creates Map with integer keys" $ do
            let instrs = [PushInt 2, PushInt 1, MakeMap 1, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "<map>\n"

        it "Gets element from Map" $ do
            let instrs = [PushInt 2, PushInt 1, MakeMap 1, PushInt 1, MapGet, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "2\n"

        it "Sets element in Map" $ do
            let instrs = [PushInt 2, PushInt 1, MakeMap 1, PushInt 1, PushInt 99, MapSet, PushInt 1, MapGet, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "99\n"

    describe "Struct Operations" $ do
        it "Creates Struct" $ do
            let instrs = [PushInt 25, PushString "age", MakeStruct 1, Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "<struct>\n"

        it "Gets field from Struct" $ do
            let instrs = [PushInt 25, PushString "age", MakeStruct 1, StructGet "age", Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "25\n"

        it "Sets field in Struct" $ do
            let instrs = [PushInt 25, PushString "age", MakeStruct 1, PushInt 30, StructSet "age", StructGet "age", Print, Halt]
            let state = newVMState instrs [] []
            output <- capture_ (execLoop state)
            output `shouldBe` "30\n"
