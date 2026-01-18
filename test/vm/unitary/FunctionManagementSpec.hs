module FunctionManagementSpec (spec) where

import Test.Hspec
import Execution.Ops.FunctionManagement
import Execution.State (VMState(..), newVMState)
import Types (Value(..), FunctionMeta(..))

spec :: Spec
spec = describe "Function Management Operations" $ do
    
    let sampleFunc = FunctionMeta { funcId = 1, funcArgCount = 2, funcAddress = 100 }
    let stateWithFunc = (newVMState [] [] [] []) { functions = [sampleFunc], ip = 10, stack = [IntVal 1, IntVal 2] }

    describe "opCall" $ do
        it "jumps to function address and saves context" $ do
            -- Call func 1 with 2 args. 
            -- Stack has [1, 2] (Top is 1? usually stack is [arg2, arg1] or [arg1, arg2] depending on conviction. Here 'take argCount' is used)
            -- If stack is [1, 2], take 2 gives [1, 2].
            let result = opCall 1 2 stateWithFunc
            case result of
                Right newState -> do
                    ip newState `shouldBe` 99 -- 100 - 1
                    curArgs newState `shouldBe` [IntVal 1, IntVal 2]
                    callStack newState `shouldBe` [(10, [])]  -- saved IP 10, saved Empty args
                    stack newState `shouldBe` [] -- Args consumed
                Left err -> expectationFailure err

        it "fails if function not found" $ do
             opCall 99 0 stateWithFunc `shouldSatisfy` isLeft

        it "fails if insufficient stack arguments" $ do
             let badState = stateWithFunc { stack = [IntVal 1] }
             opCall 1 2 badState `shouldSatisfy` isLeft

    describe "opReturn" $ do
        it "restores previous context from callStack" $ do
            let stateInFunc = (newVMState [] [] [] []) { 
                ip = 100, 
                curArgs = [IntVal 99],
                callStack = [(10, [IntVal 1000])] -- Saved return to 10, with old args [1000]
            }
            opReturn stateInFunc `shouldBe` Right (stateInFunc { ip = 10, curArgs = [IntVal 1000], callStack = [] })

        it "sets IP to end of instructions if call stack empty (return from main)" $ do
             let state = (newVMState [] [] [] []) { callStack = [] }
             opReturn state `shouldBe` Right (state { ip = -1 })

    describe "opLoadArg" $ do
        it "loads argument from curArgs onto stack" $ do
            let state = (newVMState [] [] [] []) { curArgs = [IntVal 10, IntVal 20] }
            opLoadArg 1 state `shouldBe` Right (state { stack = [IntVal 20] })

        it "fails if index out of bounds" $ do
            let state = (newVMState [] [] [] []) { curArgs = [IntVal 10] }
            opLoadArg 5 state `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
