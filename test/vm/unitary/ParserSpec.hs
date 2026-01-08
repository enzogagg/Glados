module ParserSpec (spec) where

import Test.Hspec
import Parser
import Types
import Data.Binary.Put
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = describe "Parser" $ do
    describe "Header Parsing" $ do
        it "parses valid header" $ do
            let bs = runPut $ do
                    putWord32be 0x43424300
                    putWord16be 1
                    putWord8 0
                    putWord8 0; putWord8 0; putWord8 0
            case runGetOrFail parseHeader bs of
                Right (_, _, Header mag ver flg) -> do
                    mag `shouldBe` 0x43424300
                    ver `shouldBe` 1
                    flg `shouldBe` 0
                Left _ -> expectationFailure "Failed to parse header"

        it "fails on invalid magic" $ do
            let bs = runPut $ do
                    putWord32be 0xDEADBEEF
                    putWord16be 1
                    putWord8 0
                    putWord8 0; putWord8 0; putWord8 0
            case runGetOrFail parseHeader bs of
                Left _ -> return ()
                Right _ -> expectationFailure "Should have failed"

    describe "Constant Pool Parsing" $ do
        it "parses integer constant" $ do
            let bs = runPut $ do
                    putWord32be 1 -- Count
                    putWord8 0x00 -- Tag Int
                    putWord32be 4 -- Len
                    putInt32be 42
            case runGetOrFail parseConstantPool bs of
                Right (_, _, vals) -> vals `shouldBe` [IntVal 42]
                Left _ -> expectationFailure "Failed to parse constant pool"

    describe "Instruction Parsing" $ do
        it "parses PushInt" $ do
            let bs = runPut $ do
                    putWord32be 5 -- Len of instructions
                    putWord8 0x02 -- PushInt
                    putInt32be 123
            case runGetOrFail (parseInstructions []) bs of
                Right (_, _, instrs) -> instrs `shouldBe` [PushInt 123]
                Left _ -> expectationFailure "Failed to parse instructions"

        it "parses Halt" $ do
             let bs = runPut $ do
                     putWord32be 1
                     putWord8 0xFF
             case runGetOrFail (parseInstructions []) bs of
                 Right (_, _, instrs) -> instrs `shouldBe` [Halt]
                 Left _ -> expectationFailure "Failed parsed Halt"

        it "parses all opcodes" $ do
            let bs = runPut $ do
                    putWord32be 69
                    -- Stack
                    putWord8 0x02; putInt32be 1 -- PushInt
                    putWord8 0x03; putWord32be 0 -- PushFloat (0.0)
                    putWord8 0x04; putWord8 1 -- PushBool True
                    putWord8 0x06 -- PushNil
                    putWord8 0x07 -- Pop
                    -- Math
                    putWord8 0x10 -- Add
                    putWord8 0x11 -- Sub
                    putWord8 0x12 -- Mul
                    putWord8 0x13 -- Div
                    putWord8 0x14 -- Mod
                    putWord8 0x15 -- Neg
                    -- Cmp
                    putWord8 0x20 -- Eq
                    putWord8 0x21 -- Neq
                    putWord8 0x22 -- Lt
                    putWord8 0x23 -- Gt
                    putWord8 0x24 -- Le
                    putWord8 0x25 -- Ge
                    -- List
                    putWord8 0x30 -- Cons
                    putWord8 0x31 -- Head
                    putWord8 0x32 -- Tail
                    putWord8 0x33; putInt32be 0 -- ListMake
                    putWord8 0x34 -- Len
                    -- IO
                    putWord8 0x80 -- Print
                    putWord8 0x81 -- Input
                    -- Jumps
                    putWord8 0x60; putInt32be 10 -- Jump
                    putWord8 0x61; putInt32be 20 -- JumpIfTrue
                    putWord8 0x62; putInt32be 30 -- JumpIfFalse
                    -- Funcs
                    putWord8 0x70; putInt32be 1; putWord8 0 -- Call
                    putWord8 0x71 -- Return
                    putWord8 0x72; putInt32be 2 -- Closure
                    putWord8 0x73; putInt32be 0 -- LoadArg

            case runGetOrFail (parseInstructions []) bs of
                Right (_, _, instrs) -> do
                    length instrs `shouldBe` 31
                    head instrs `shouldBe` PushInt 1
                    last instrs `shouldBe` LoadArg 0
                Left (_, _, err) -> expectationFailure $ "Failed parse all: " ++ err

    describe "Function Table Parsing" $ do
        it "parses function table" $ do
            let bs = runPut $ do
                    putWord32be 1 -- Count
                    putWord32be 0 -- Idx
                    putWord32be 10 -- Addr
                    putWord8 2 -- ArgC
            case runGetOrFail parseFunctionTable bs of
                 Right (_, _, funcs) -> funcs `shouldBe` [FunctionMeta 0 10 2]
                 Left _ -> expectationFailure "Failed to parse function table"
