{-# LANGUAGE OverloadedStrings #-}
module BytecodeGeneratorSpec (spec) where

import Test.Hspec
import BytecodeGenerator (encodeBytecodeFile)
import Bytecode
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = describe "BytecodeGenerator" $ do
    
    it "encodes header correctly" $ do
        let file = BytecodeFile 0x43424300 1 0 [] [] []
        let bs = encodeBytecodeFile file
        let bytes = BL.unpack bs
        -- Magic: 43 42 43 00 (CBC\0)
        take 4 bytes `shouldBe` [0x43, 0x42, 0x43, 0x00]
        -- Version: 00 01
        take 2 (drop 4 bytes) `shouldBe` [0x00, 0x01]

    it "encodes integer constant" $ do
        let file = BytecodeFile 0 0 0 [BCInt 42] [] []
        let bs = encodeBytecodeFile file
        -- Const Pool Size: 1 (4 bytes)
        -- Type: 0 (1 byte)
        -- Len: 4 (4 bytes)
        -- Val: 42 (4 bytes)
        -- Header is 4+2+1+3(pad) = 10 bytes?
        -- putHeader: Mag(4) + Ver(2) + Flg(1) + Pad(3) = 10 bytes.
        let poolStart = BL.drop 10 bs
        -- Pool Count: 00 00 00 01
        BL.take 4 poolStart `shouldBe` BL.pack [0,0,0,1]
        
        let firstConst = BL.drop 4 poolStart
        -- Type 0 (Int)
        BL.take 1 firstConst `shouldBe` BL.pack [0]

    it "encodes instruction PushInt" $ do
        let file = BytecodeFile 0 0 0 [] [] [PushInt 5]
        let bs = encodeBytecodeFile file
        -- Instructions follow header + pool + func table
        -- Pool count 0 (4 bytes). Func count 0 (4 bytes). 
        -- Header 10 + 4 + 4 = 18 bytes.
        let instStart = BL.drop 18 bs
        -- Inst data length (4 bytes) + insts
        let body = BL.drop 4 instStart
        
        -- PushInt opcode 0x02 + 4 bytes value
        BL.take 1 body `shouldBe` BL.pack [0x02]
        BL.take 4 (BL.drop 1 body) `shouldBe` BL.pack [0,0,0,5]

    it "encodes Halt" $ do
        let file = BytecodeFile 0 0 0 [] [] [Halt]
        let bs = encodeBytecodeFile file
        let instStart = BL.drop 18 bs
        let body = BL.drop 4 instStart
        BL.take 1 body `shouldBe` BL.pack [0xFF]
