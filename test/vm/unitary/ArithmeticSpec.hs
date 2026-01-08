module ArithmeticSpec (spec) where

import Test.Hspec
import Execution.Ops.ArithmeticOperations
import Execution.State (VMState(..), newVMState)
import Types (Value(..))

spec :: Spec
spec = describe "Arithmetic Operations" $ do
    
    -- ADDITION
    describe "opAdd" $ do
        it "adds two integers" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 3, IntVal 2] }
            opAdd state `shouldBe` Right (state { stack = [IntVal 5] })

        it "adds two floats" $ do
            let state = (newVMState [] [] []) { stack = [FloatVal 1.5, FloatVal 2.5] }
            opAdd state `shouldBe` Right (state { stack = [FloatVal 4.0] })

        it "adds int and float (int promoted)" $ do
            let state = (newVMState [] [] []) { stack = [FloatVal 1.5, IntVal 2] }
            opAdd state `shouldBe` Right (state { stack = [FloatVal 3.5] })
        
        it "adds float and int (int promoted)" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 2, FloatVal 1.5] }
            opAdd state `shouldBe` Right (state { stack = [FloatVal 3.5] })

        it "fails with insufficient stack" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 1] }
            opAdd state `shouldSatisfy` isLeft

        it "fails with invalid types" $ do
            let state = (newVMState [] [] []) { stack = [StringVal "a", IntVal 1] }
            opAdd state `shouldSatisfy` isLeft

    -- SUBTRACTION
    describe "opSub" $ do
        it "subtracts integers (b - a)" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 3, IntVal 10] } -- stack: [top, below] -> 10 - 3 = 7
            opSub state `shouldBe` Right (state { stack = [IntVal 7] })

        it "subtracts floats" $ do
            let state = (newVMState [] [] []) { stack = [FloatVal 1.5, FloatVal 3.5] }
            opSub state `shouldBe` Right (state { stack = [FloatVal 2.0] })

    -- MULTIPLICATION
    describe "opMul" $ do
        it "multiplies integers" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 3, IntVal 4] }
            opMul state `shouldBe` Right (state { stack = [IntVal 12] })

    -- DIVISION
    describe "opDiv" $ do
        it "divides integers" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 2, IntVal 10] } -- 10 / 2
            opDiv state `shouldBe` Right (state { stack = [IntVal 5] })

        it "fails on division by zero (int)" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 0, IntVal 10] }
            opDiv state `shouldSatisfy` isLeft

        it "divides floats" $ do
            let state = (newVMState [] [] []) { stack = [FloatVal 2.0, FloatVal 5.0] }
            opDiv state `shouldBe` Right (state { stack = [FloatVal 2.5] })

        it "fails on division by zero (float)" $ do
             let state = (newVMState [] [] []) { stack = [FloatVal 0.0, FloatVal 5.0] }
             opDiv state `shouldSatisfy` isLeft

    -- MODULO
    describe "opMod" $ do
        it "computes modulo of integers" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 3, IntVal 10] } -- 10 % 3 = 1
            opMod state `shouldBe` Right (state { stack = [IntVal 1] })

        it "fails on modulo by zero" $ do
            let state = (newVMState [] [] []) { stack = [IntVal 0, IntVal 10] }
            opMod state `shouldSatisfy` isLeft

        it "fails on non-integer types for modulo" $ do
            let state = (newVMState [] [] []) { stack = [FloatVal 2.0, FloatVal 5.0] }
            opMod state `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
