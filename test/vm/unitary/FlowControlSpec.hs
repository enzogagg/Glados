module FlowControlSpec (spec) where

import Test.Hspec
import Execution.Ops.FlowControl
import Execution.State (VMState(..), newVMState)
import Types (Value(..))

spec :: Spec
spec = describe "Flow Control Operations" $ do
    
    -- JUMP
    describe "opJump" $ do
        it "sets IP to address - 1" $ do
            let state = (newVMState [] [] [] []) { ip = 10 }
            let dest = 50
            opJump dest state `shouldBe` Right (state { ip = 49 })

    -- JUMP IF FALSE
    describe "opJumpIfFalse" $ do
        it "jumps if top of stack is False (and consumes it)" $ do
            let state = (newVMState [] [] [] []) { ip = 10, stack = [BoolVal False, IntVal 42] }
            let dest = 20
            opJumpIfFalse dest state `shouldBe` Right (state { ip = 19, stack = [IntVal 42] })

        it "does NOT jump if top of stack is True (consumes it)" $ do
            let state = (newVMState [] [] [] []) { ip = 10, stack = [BoolVal True, IntVal 42] }
            let dest = 20
            -- Should stay at ip 10 (loop will increment it later)
            opJumpIfFalse dest state `shouldBe` Right (state { ip = 10, stack = [IntVal 42] })
        
        it "fails if stack is empty" $ do
             let state = (newVMState [] [] [] []) { stack = [] }
             opJumpIfFalse 10 state `shouldSatisfy` isLeft

        it "fails if top is not boolean" $ do
             let state = (newVMState [] [] [] []) { stack = [IntVal 1] }
             opJumpIfFalse 10 state `shouldSatisfy` isLeft

    -- JUMP IF TRUE
    describe "opJumpIfTrue" $ do
        it "jumps if top of stack is True" $ do
            let state = (newVMState [] [] [] []) { ip = 10, stack = [BoolVal True] }
            opJumpIfTrue 20 state `shouldBe` Right (state { ip = 19, stack = [] })

        it "does not jump if top is False" $ do
            let state = (newVMState [] [] [] []) { ip = 10, stack = [BoolVal False] }
            opJumpIfTrue 20 state `shouldBe` Right (state { ip = 10, stack = [] })

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
