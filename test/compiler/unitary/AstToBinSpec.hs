{-# LANGUAGE ScopedTypeVariables #-}
module AstToBinSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Control.Monad (when, replicateM, replicateM_)
import System.Directory (removeFile, doesFileExist)
import Control.Exception (catch, SomeException, finally)

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

parseHeader :: Get (Word32, Word16, Word8)
parseHeader = do
    magic <- getWord32be
    ver <- getWord16be
    flags <- getWord8
    skip 3 -- Reserved
    return (magic, ver, flags)

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

shouldHaveOpcode :: FilePath -> Word8 -> Expectation
shouldHaveOpcode file_name opcode = do
    content <- BL.fromStrict <$> BS.readFile file_name
    let instrs = getInstructions content
    (opcode `elem` instrs) `shouldBe` True

-- ==========================
-- Tests
-- ==========================

spec :: Spec
spec = do
    describe "Compiler AstToBin - Binary Generation" $ do

        -- ==========================
        -- PARTIE 1: Structure du Binaire
        -- ==========================
        describe "Part 1: Binary Structure" $ do

            it "should generate a valid HEADER (Magic, Version, Flags)" $ do
                let file_name = "struct_header.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram []) file_name
                    content <- BS.readFile file_name
                    let (magic, version, flags) = runGet parseHeader (BL.fromStrict content)
                    magic `shouldBe` 0x43424300 -- CBC\0
                    version `shouldBe` 0x0100   -- 1.0
                    flags `shouldBe` 0x00

            it "should generate a valid CONSTANT POOL" $ do
                let file_name = "struct_const.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IAString "test", IASymbol "sym"]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let pb = runGet parseFullBinary content
                    pbConstPoolCount pb `shouldSatisfy` (>= 2)

            it "should generate a valid FUNCTION TABLE" $ do
                let file_name = "struct_func.cbc"
                withTestFile file_name $ do
                    let ast = IAProgram [IAFunctionDef "myFunc" [] Nothing []]
                    _ <- parseBin ast file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let pb = runGet parseFullBinary content
                    pbFuncTableCount pb `shouldBe` 1

            it "should generate a valid INSTRUCTIONS section" $ do
                let file_name = "struct_instr.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IANumber 1]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let pb = runGet parseFullBinary content
                    length (pbInstructions pb) `shouldBe` 6

        -- ==========================
        -- PARTIE 2: Tests des Opcodes
        -- ==========================
        describe "Part 2: Opcodes Generation" $ do

            describe "Value Management" $ do
                it "Push Int (0x02)" $ do
                    let file_name = "op_int.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IANumber 42]) file_name
                        content <- BL.fromStrict <$> BS.readFile file_name
                        getInstructions content `shouldBe` [0x02, 0, 0, 0, 42, 0xFF]

                it "Push Float (0x03)" $ do
                    let file_name = "op_float.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAFloatLiteral 1.5]) file_name
                        content <- BL.fromStrict <$> BS.readFile file_name
                        getInstructions content `shouldBe` [0x03, 0x3F, 0xC0, 0, 0, 0xFF]

                it "Push Bool (0x04)" $ do
                    let file_name = "op_bool.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IABoolean True]) file_name
                        content <- BL.fromStrict <$> BS.readFile file_name
                        getInstructions content `shouldBe` [0x04, 1, 0xFF]

                it "Push String (0x01 via ConstPool)" $ do
                    let file_name = "op_str.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAString "s"]) file_name
                        content <- BL.fromStrict <$> BS.readFile file_name
                        getInstructions content `shouldBe` [0x01, 0, 0, 0, 0, 0xFF]

                it "Push Nil (0x06)" $ do
                    let file_name = "op_nil.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAUnit]) file_name
                        content <- BL.fromStrict <$> BS.readFile file_name
                        getInstructions content `shouldBe` [0x06, 0xFF]

            describe "Arithmetic Operations" $ do
                it "Add (0x10), Sub (0x11), Mul (0x12), Div (0x13), Mod (0x14)" $ do
                    let file_name = "op_arith.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "+" (IANumber 2)]) file_name
                        file_name `shouldHaveOpcode` 0x10
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "-" (IANumber 2)]) file_name
                        file_name `shouldHaveOpcode` 0x11
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "*" (IANumber 2)]) file_name
                        file_name `shouldHaveOpcode` 0x12
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "/" (IANumber 2)]) file_name
                        file_name `shouldHaveOpcode` 0x13
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "%" (IANumber 2)]) file_name
                        file_name `shouldHaveOpcode` 0x14

            describe "Comparison Operations" $ do
                it "Eq(0x20), Neq(0x21), Lt(0x22), Gt(0x23), Lte(0x24), Gte(0x25)" $ do
                    let file_name = "op_cmp.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "==" (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x20
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "!=" (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x21
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "<" (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x22
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) ">" (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x23
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) "<=" (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x24
                        _ <- parseBin (IAProgram [IAInfix (IANumber 1) ">=" (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x25

            describe "Logic Operations" $ do
                it "And(0x26), Or(0x27), Not(0x28)" $ do
                    let file_name = "op_logic.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAInfix (IABoolean True) "et" (IABoolean False)]) file_name
                        file_name `shouldHaveOpcode` 0x26
                        _ <- parseBin (IAProgram [IAInfix (IABoolean True) "ou" (IABoolean False)]) file_name
                        file_name `shouldHaveOpcode` 0x27
                        _ <- parseBin (IAProgram [IACall "!" [IABoolean True]]) file_name
                        file_name `shouldHaveOpcode` 0x28

            describe "Variable Operations" $ do
                it "Load(0x50), Store(0x51), Define(0x52)" $ do
                    let file_name = "op_vars.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IASymbol "x"]) file_name
                        file_name `shouldHaveOpcode` 0x50

                        _ <- parseBin (IAProgram [IAAssign "x" (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x51

                        _ <- parseBin (IAProgram [IADeclare "x" Nothing (IANumber 1)]) file_name
                        file_name `shouldHaveOpcode` 0x52

            describe "Control Flow Operations" $ do
                it "JmpIfFalse(0x62), Jmp(0x60)" $ do
                    let file_name = "op_jmp.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAIf (IABoolean True) [IANumber 1] Nothing]) file_name
                        file_name `shouldHaveOpcode` 0x62

                        _ <- parseBin (IAProgram [IAWhile (IABoolean False) [IANumber 1]]) file_name
                        file_name `shouldHaveOpcode` 0x62
                        file_name `shouldHaveOpcode` 0x60

            describe "IO Operations" $ do
                it "Print(0x80), Input(0x81)" $ do
                    let file_name = "op_io.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IACall "afficher" [IANumber 1]]) file_name
                        file_name `shouldHaveOpcode` 0x80

                        _ <- parseBin (IAProgram [IACall "ecouter" []]) file_name
                        file_name `shouldHaveOpcode` 0x81

            describe "Function Operations" $ do
                it "Call(0x70), Return(0x71), Closure(0x72), LoadArg(0x73)" $ do
                    let file_name = "op_func.cbc"
                    withTestFile file_name $ do
                        let validProgram = IAProgram [
                                IAFunctionDef "f" [("arg", Just IntT)] Nothing [IAReturn (IASymbol "arg")],
                                IACall "f" [IANumber 1]
                                ]
                        _ <- parseBin validProgram file_name

                        file_name `shouldHaveOpcode` 0x70 -- Call
                        file_name `shouldHaveOpcode` 0x71 -- Return
                        file_name `shouldHaveOpcode` 0x72 -- Closure
                        file_name `shouldHaveOpcode` 0x73 -- LoadArg

            describe "Data Structure Operations" $ do
                it "Tuple(0x90), TupleGet(0x91)" $ do
                    let file_name = "op_tuple.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IATuple [IANumber 1]]) file_name
                        file_name `shouldHaveOpcode` 0x90

                        _ <- parseBin (IAProgram [IACall "get" [IATuple [], IANumber 0]]) file_name
                        file_name `shouldHaveOpcode` 0x91

                it "List Ops: List(0x33), Cons(0x30), Head(0x31)..." $ do
                     let file_name = "op_list.cbc"
                     withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IAList []]) file_name
                        file_name `shouldHaveOpcode` 0x33

                        _ <- parseBin (IAProgram [IACall "cons" [IANumber 1, IAList []]]) file_name
                        file_name `shouldHaveOpcode` 0x30

                        _ <- parseBin (IAProgram [IACall "head" [IAList []]]) file_name
                        file_name `shouldHaveOpcode` 0x31

                it "Array(0x92), ArrayGet(0x93), ArraySet(0x94)" $ do
                    let file_name = "op_array.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IACall "vector" [IANumber 1]]) file_name
                        file_name `shouldHaveOpcode` 0x92

                        _ <- parseBin (IAProgram [IACall "get" [IACall "vector" [], IANumber 0]]) file_name
                        file_name `shouldHaveOpcode` 0x93

                        _ <- parseBin (IAProgram [IACall "set" [IACall "vector" [], IANumber 0, IANumber 1]]) file_name
                        file_name `shouldHaveOpcode` 0x94

                it "Map(0x95), MapGet(0x96), MapSet(0x97)" $ do
                    let file_name = "op_map.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IACall "dico" [IAString "k", IANumber 1]]) file_name
                        file_name `shouldHaveOpcode` 0x95

                        _ <- parseBin (IAProgram [IACall "get" [IACall "dico" [], IAString "k"]]) file_name
                        file_name `shouldHaveOpcode` 0x96

                        _ <- parseBin (IAProgram [IACall "set" [IACall "dico" [], IAString "k", IANumber 2]]) file_name
                        file_name `shouldHaveOpcode` 0x97

                it "Struct(0x98), StructGet(0x99), StructSet(0x9A)" $ do
                     let file_name = "op_struct.cbc"
                     withTestFile file_name $ do
                         _ <- parseBin (IAProgram [IACall "struct" [IAString "field", IANumber 1]]) file_name
                         file_name `shouldHaveOpcode` 0x98

                         _ <- parseBin (IAProgram [IACall "get" [IACall "struct" [], IAString "field"]]) file_name
                         file_name `shouldHaveOpcode` 0x99

                         _ <- parseBin (IAProgram [IACall "set" [IACall "struct" [], IAString "field", IANumber 2]]) file_name
                         file_name `shouldHaveOpcode` 0x9A

            describe "File Operations" $ do
                it "Open(0xA0), Read(0xA1), Write(0xA2), Close(0xA3)" $ do
                    let file_name = "op_file.cbc"
                    withTestFile file_name $ do
                        _ <- parseBin (IAProgram [IACall "ouvrir" [IAString "f", IAString "r"]]) file_name
                        file_name `shouldHaveOpcode` 0xA0

                        _ <- parseBin (IAProgram [IACall "lire" [IAUnit]]) file_name
                        file_name `shouldHaveOpcode` 0xA1

                        _ <- parseBin (IAProgram [IACall "ecrire" [IAUnit, IAString "c"]]) file_name
                        file_name `shouldHaveOpcode` 0xA2

                        _ <- parseBin (IAProgram [IACall "fermer" [IAUnit]]) file_name
                        file_name `shouldHaveOpcode` 0xA3


            it "should compile Print" $ do
                let file_name = "test_print.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IACall "afficher" [IANumber 42]]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    let instrNoHalt = init instrs
                    last instrNoHalt `shouldBe` 0x80

            it "should compile Input" $ do
                let file_name = "test_input.cbc"
                withTestFile file_name $ do
                    _ <- parseBin (IAProgram [IACall "ecouter" []]) file_name
                    content <- BL.fromStrict <$> BS.readFile file_name
                    let instrs = getInstructions content
                    instrs `shouldBe` [0x81, 0xFF]

        describe "Advanced Types" $ do
             it "should compile List creation" $ do
                 let file_name = "test_list.cbc"
                 withTestFile file_name $ do
                     _ <- parseBin (IAProgram [IAList [IANumber 1, IANumber 2]]) file_name
                     content <- BL.fromStrict <$> BS.readFile file_name
                     let instrs = getInstructions content
                     drop 10 instrs `shouldBe` [0x33, 0, 0, 0, 2, 0xFF]

             it "should compile Tuple creation" $ do
                 let file_name = "test_tuple.cbc"
                 withTestFile file_name $ do
                     _ <- parseBin (IAProgram [IATuple [IANumber 1, IANumber 2]]) file_name
                     content <- BL.fromStrict <$> BS.readFile file_name
                     let instrs = getInstructions content
                     drop 10 instrs `shouldBe` [0x90, 0, 0, 0, 2, 0xFF]
