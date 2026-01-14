{-# LANGUAGE ScopedTypeVariables #-}
module AstToBinSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Control.Monad.State
import Control.Monad (when, replicateM, replicateM_)
import qualified Data.Map.Strict as Map
import System.Directory (removeFile, doesFileExist)
import Control.Exception (catch, SomeException, finally)
import System.IO.Unsafe (unsafePerformIO)

import Types
import AstToBin

-- ==========================
-- Helper pour nettoyer les fichiers de test
-- ==========================

cleanupTestFile :: FilePath -> IO ()
cleanupTestFile path = do
    exists <- doesFileExist path
    when exists
        $ removeFile path `catch` (\ (_ :: SomeException) -> return ())

withTestFile :: FilePath -> IO a -> IO a
withTestFile path action = action `finally` cleanupTestFile path

-- ==========================
-- Helpers pour parser le bytecode généré (reverse engineering)
-- ==========================

-- Check Header: Magic(4) + Version(2) + Flags(1) + Reserved(3) = 10 bytes
parseHeader :: Get (Word32, Word16, Word8)
parseHeader = do
    magic <- getWord32be
    ver <- getWord16be
    flags <- getWord8
    skip 3 -- Reserved
    return (magic, ver, flags)

-- Structure complète pour vérification approfondie
data ParsedBinary = ParsedBinary {
    pbMagic :: Word32,
    pbVersion :: Word16,
    pbFlags :: Word8,
    pbConstPoolCount :: Int,
    pbFuncTableCount :: Int,
    pbInstructions :: [Word8]
} deriving (Show)

parseFullBinary :: Get ParsedBinary
parseFullBinary = do
    (magic, ver, flags) <- parseHeader

    poolCount <- getWord32be
    replicateM_ (fromIntegral poolCount) $ do
        _ <- getWord8
        len <- getWord32be
        skip (fromIntegral len)

    funcCount <- getWord32be
    replicateM_ (fromIntegral funcCount) $ do
        skip 4 -- Index
        skip 4 -- Address
        skip 1 -- ArgCount

    instrCount <- getWord32be
    instrs <- replicateM (fromIntegral instrCount) getWord8

    return $ ParsedBinary magic ver flags (fromIntegral poolCount) (fromIntegral funcCount) instrs

getInstructions :: BL.ByteString -> [Word8]
getInstructions bs = pbInstructions $ runGet parseFullBinary bs

-- ==========================
-- Tests
-- ==========================

spec :: Spec
spec = do
    describe "Compiler AstToBin - Binary Generation" $ do

        describe "Header Generation" $ do
            it "should generate a valid header (Magic CBC)" $ do
                let file_name = "test_header.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram []) file_name
                    content <- BS.readFile file_name
                    let (magic, version, flags) = runGet parseHeader (BL.fromStrict content)
                    magic `shouldBe` 0x43424300 -- CBC\0
                    version `shouldBe` 0x0100   -- 1.0
                    flags `shouldBe` 0x00

        describe "Basic Opcodes Generation" $ do

            it "should compile Integer (PUSH_INT)" $ do
                let file_name = "test_int.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IANumber 42]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH_INT (02) + 42 (00 00 00 2A) + HALT (FF)
                    instrs `shouldBe` [0x02, 0x00, 0x00, 0x00, 0x2A, 0xFF]

            it "should compile Float (PUSH_FLOAT)" $ do
                let file_name = "test_float.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAFloatLiteral 2.5]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH_FLOAT (03) + 2.5 (IEEE 40 20 00 00) + HALT (FF)
                    instrs `shouldBe` [0x03, 0x40, 0x20, 0x00, 0x00, 0xFF]

            it "should compile Boolean (PUSH_BOOL)" $ do
                let file_name = "test_bool.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IABoolean True, IABoolean False]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH_BOOL (04) + 1, PUSH_BOOL (04) + 0 + HALT (FF)
                    instrs `shouldBe` [0x04, 0x01, 0x04, 0x00, 0xFF]

            it "should compile Nil (PUSH_NIL)" $ do
                let file_name = "test_nil.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAUnit]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH_NIL (06) + HALT (FF)
                    instrs `shouldBe` [0x06, 0xFF]

            it "should compile String (PUSH_STRING via Const Pool)" $ do
                let file_name = "test_str.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAString "Hello"]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH_CONST (01) + Index 0 (00 00 00 00) + HALT (FF)
                    instrs `shouldBe` [0x01, 0x00, 0x00, 0x00, 0x00, 0xFF]
                    -- check const pool count is 1
                    let pb = runGet parseFullBinary content
                    pbConstPoolCount pb `shouldBe` 1

        describe "Arithmetic Operations" $ do

            it "should compile ADD (+)" $ do
                let file_name = "test_add.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAInfix (IANumber 1) "+" (IANumber 2)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 1, PUSH 2, ADD (10) + HALT (FF)
                    instrs `shouldBe` [0x02, 0, 0, 0, 1, 0x02, 0, 0, 0, 2, 0x10, 0xFF]

            it "should compile SUB (-)" $ do
                let file_name = "test_sub.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAInfix (IANumber 1) "-" (IANumber 2)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 1, PUSH 2, SUB (11) + HALT (FF)
                    instrs `shouldBe` [0x02, 0, 0, 0, 1, 0x02, 0, 0, 0, 2, 0x11, 0xFF]

            it "should compile MUL (*)" $ do
                let file_name = "test_mul.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAInfix (IANumber 2) "*" (IANumber 3)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 2, PUSH 3, MUL (12) + HALT (FF)
                    instrs `shouldBe` [0x02, 0, 0, 0, 2, 0x02, 0, 0, 0, 3, 0x12, 0xFF]

            it "should compile DIV (/)" $ do
                let file_name = "test_div.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAInfix (IANumber 10) "/" (IANumber 2)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 10, PUSH 2, DIV (13) + HALT (FF)
                    instrs `shouldBe` [0x02, 0, 0, 0, 10, 0x02, 0, 0, 0, 2, 0x13, 0xFF]

            it "should compile MOD (%)" $ do
                let file_name = "test_mod.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAInfix (IANumber 10) "%" (IANumber 3)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 10, PUSH 3, MOD (14) + HALT (FF)
                    instrs `shouldBe` [0x02, 0, 0, 0, 10, 0x02, 0, 0, 0, 3, 0x14, 0xFF]

        describe "Comparison Operations" $ do

            it "should compile EQ (==)" $ do
                let file_name = "test_eq.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAInfix (IANumber 1) "==" (IANumber 1)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 1, PUSH 1, EQ (20), HALT
                    let instrNoHalt = init instrs
                    last instrNoHalt `shouldBe` 0x20

            it "should compile NEQ (!=)" $ do
                let file_name = "test_neq.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAInfix (IANumber 1) "!=" (IANumber 2)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 1, PUSH 2, NEQ (21), HALT
                    let instrNoHalt = init instrs
                    last instrNoHalt `shouldBe` 0x21

        describe "Logic Operations" $ do
            it "should compile AND (et)" $ do
                let file_name = "test_and.cbc"
                withTestFile file_name $ do
                   _ <- parseBin (IAProgram [IAInfix (IABoolean True) "et" (IABoolean False)]) file_name
                   content <- BL.fromStrict <$> BS.readFile file_name
                   let instrs = getInstructions content
                   -- ..., AND (26), HALT
                   let instrNoHalt = init instrs
                   last instrNoHalt `shouldBe` 0x26

            it "should compile OR (ou)" $ do
                let file_name = "test_or.cbc"
                withTestFile file_name $ do
                   _ <- parseBin (IAProgram [IAInfix (IABoolean True) "ou" (IABoolean False)]) file_name
                   content <- BL.fromStrict <$> BS.readFile file_name
                   let instrs = getInstructions content
                   -- ..., OR (27), HALT
                   let instrNoHalt = init instrs
                   last instrNoHalt `shouldBe` 0x27

             -- Note: NOT Opcode is usually a function call like !(x) in CLaD
            it "should compile NOT (Call !)" $ do
                let file_name = "test_not.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IACall "!" [IABoolean True]]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- ..., NOT (28), HALT
                    let instrNoHalt = init instrs
                    last instrNoHalt `shouldBe` 0x28

        describe "Variable Management" $ do

            it "should compile variable declaration (DEFINE via Symbol)" $ do
                let file_name = "test_def.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IADeclare "x" (Just IntT) (IANumber 10)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 10, DEFINE (52) + Index
                    let opcode = instrs !! 5
                    opcode `shouldBe` 0x52

            it "should compile variable assignment (STORE)" $ do
                let file_name = "test_store.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAAssign "x" (IANumber 20)]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 20, STORE (51) + Index
                    let opcode = instrs !! 5
                    opcode `shouldBe` 0x51

            it "should compile variable usage (LOAD)" $ do
                let file_name = "test_load.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IASymbol "x"]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- LOAD (50) + Index (00 00 00 00), HALT
                    instrs `shouldBe` [0x50, 0, 0, 0, 0, 0xFF]

        describe "Control Structures" $ do
             -- Les structures de contrôle (If, While) génèrent des JMP.

            it "should use JMP_IF_FALSE and JMP for If-Else" $ do
                let file_name = "test_if.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAIf (IABoolean True) [IANumber 1] (Just [IANumber 2])]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- Doit contenir JMP_IF_FALSE (62)
                    (0x62 `elem` instrs) `shouldBe` True

            it "should use JMP_IF_FALSE and JMP for While" $ do
                let file_name = "test_while.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAWhile (IABoolean True) [IANumber 1]]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- Doit contenir JMP_IF_FALSE (62) et JMP (60)
                    (0x62 `elem` instrs) `shouldBe` True
                    (0x60 `elem` instrs) `shouldBe` True

        describe "Builtin Calls" $ do

            it "should compile Print" $ do
                let file_name = "test_print.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IACall "afficher" [IANumber 42]]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- PUSH 42, PRINT (80), HALT
                    let instrNoHalt = init instrs
                    last instrNoHalt `shouldBe` 0x80

            it "should compile Input" $ do
                let file_name = "test_input.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IACall "ecouter" []]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    -- INPUT (81), HALT
                    instrs `shouldBe` [0x81, 0xFF]

        describe "Advanced Types" $ do
             it "should compile List creation" $ do
                 let file_name = "test_list.cbc"
                 withTestFile file_name $ do
                     _ <- parseBin (IAProgram [IAList [IANumber 1, IANumber 2]]) file_name
                     content <- BL.fromStrict <$> BS.readFile file_name
                     let instrs = getInstructions content
                     -- PUSH 1, PUSH 2, LIST (33) + Size(2)
                     -- PUSH 1 (5 bytes) + PUSH 2 (5 bytes) = 10 bytes
                     -- LIST 33 + 4 bytes size, HALT
                     drop 10 instrs `shouldBe` [0x33, 0, 0, 0, 2, 0xFF]

             it "should compile Tuple creation" $ do
                 let file_name = "test_tuple.cbc"
                 withTestFile file_name $ do
                     _ <- parseBin (IAProgram [IATuple [IANumber 1, IANumber 2]]) file_name
                     content <- BL.fromStrict <$> BS.readFile file_name
                     let instrs = getInstructions content
                     -- PUSH 1, PUSH 2, MAKE_TUPLE (90) + Size(2), HALT
                     drop 10 instrs `shouldBe` [0x90, 0, 0, 0, 2, 0xFF]
