module CompilerSpec (spec) where

import Test.Hspec
import Compiler (compileProgram)
import Types
import Bytecode

spec :: Spec
spec = describe "Compiler" $ do
    
    describe "Literals" $ do
        it "compiles integer" $ do
            let ast = IAProgram [IANumber 42]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushInt 42]

        it "compiles float" $ do
            let ast = IAProgram [IAFloatLiteral 3.14]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushFloat 3.14]

        it "compiles boolean" $ do
            let ast = IAProgram [IABoolean True]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushBool True]

        it "compiles string" $ do
            let ast = IAProgram [IAString "hello"]
            let (BytecodeFile _ _ _ consts _ _) = compileProgram ast
            consts `shouldContain` [BCString "hello"]
            -- Should check if PushString refers to correct index, but containment is a good start

    describe "Operations" $ do
        it "compiles addition" $ do
            let ast = IAProgram [IAInfix (IANumber 1) "+" (IANumber 2)]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushInt 1, PushInt 2, Add]

        it "compiles comparison" $ do
            let ast = IAProgram [IAInfix (IANumber 1) "<" (IANumber 2)]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushInt 1, PushInt 2, Lt]

    describe "Flow Control" $ do
        it "compiles condition (If)" $ do
            let ast = IAProgram [IAIf (IABoolean True) (IANumber 1) (IANumber 2)]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            -- Verify structure: PushBool, JumpFalse L1, PushInt 1, Jump L2, Label L1, PushInt 2, Label L2
            -- Included Main Jump over functions + If Jump = 2 Jumps
            length [ i | i <- instrs, case i of JumpIfFalse _ -> True; _ -> False] `shouldBe` 1
            length [ i | i <- instrs, case i of Jump _ -> True; _ -> False] `shouldBe` 2

    describe "Loops" $ do
        it "compiles while loop" $ do
            let ast = IAProgram [IAWhile (IABoolean True) [IANumber 1]]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            -- Structure: Label Start, PushBool, JumpFalse End, PushInt 1, Jump Start, Label End
            length [ i | i <- instrs, case i of JumpIfFalse _ -> True; _ -> False] `shouldBe` 1
            -- Jump Start + Main Jump = 2 Jumps
            length [ i | i <- instrs, case i of Jump _ -> True; _ -> False] `shouldBe` 2

        it "compiles for loop" $ do
            let ast = IAProgram [IAFor (IANumber 0) (IABoolean True) (IANumber 2) [IANumber 1]]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            -- Init (PushInt), Label, Cond (PushBool), JumpFalse, Body (PushInt), Update (PushInt), Jump, Label
            -- JumpIfFalse: 1. Jump: 1 + 1 (Main) = 2.
            length [ i | i <- instrs, case i of JumpIfFalse _ -> True; _ -> False] `shouldBe` 1
            length [ i | i <- instrs, case i of Jump _ -> True; _ -> False] `shouldBe` 2

    describe "Variables and Flow" $ do
        it "compiles declaration" $ do
            let ast = IAProgram [IADeclare "x" Nothing (IANumber 10)]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            -- PushInt 10, Define "x" (index)
            instrs `shouldContain` [PushInt 10]
            length [ i | i <- instrs, case i of Define _ -> True; _ -> False] `shouldBe` 1

        it "compiles assignment" $ do
            let ast = IAProgram [IAAssign "x" (IANumber 20)]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushInt 20]
            length [ i | i <- instrs, case i of Store _ -> True; _ -> False] `shouldBe` 1

        it "compiles symbol load (global)" $ do
            let ast = IAProgram [IASymbol "x"]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            length [ i | i <- instrs, case i of Load _ -> True; _ -> False] `shouldBe` 1

        it "compiles return" $ do
            let ast = IAProgram [IAReturn (IANumber 5)]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushInt 5, Return]

    describe "Functions" $ do
        it "compiles function call" $ do
            let ast = IAProgram [IACall "print" [IANumber 42]]
            let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
            instrs `shouldContain` [PushInt 42, Print]
            
        it "compiles function definition" $ do
             -- This implies Jump over function body
             let ast = IAProgram [IAFunctionDef "foo" [] Nothing [IANumber 1]]
             let (BytecodeFile _ _ _ _ _ instrs) = compileProgram ast
             -- Jump Main, Label Func, PushInt 1, Return, Label Main, Halt
             -- Jump: 1 (Main). Return: 1.
             length [ i | i <- instrs, case i of Jump _ -> True; _ -> False] `shouldBe` 1
             instrs `shouldContain` [Return]
