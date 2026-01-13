{-# LANGUAGE ScopedTypeVariables #-}
module AstToBinSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Control.Monad.State
import qualified Data.Map.Strict as Map
import System.Directory (removeFile, doesFileExist)
import Control.Exception (catch, SomeException)

import Types
import AstToBin

-- ==========================
-- Helper pour nettoyer les fichiers de test
-- ==========================

cleanupTestFile :: FilePath -> IO ()
cleanupTestFile path = do
    exists <- doesFileExist path
    Control.Monad.when exists
        $ removeFile path `catch` (\ (_ :: SomeException) -> return ())

-- Liste de tous les fichiers de test créés
testFiles :: [FilePath]
testFiles = [
    "test_header.cbc", "test_header_size.cbc", "test_empty_pool.cbc",
    "test_string_pool.cbc", "test_symbol_pool.cbc", "test_dedup.cbc",
    "test_no_func.cbc", "test_one_func.cbc", "test_multi_func.cbc",
    "test_push_int.cbc", "test_push_bool.cbc", "test_add.cbc",
    "test_print.cbc", "test_halt.cbc", "test_if.cbc", "test_if_else.cbc",
    "test_closure.cbc", "test_call.cbc", "test_return.cbc",
    "test_load_arg.cbc", "test_define.cbc", "test_store.cbc",
    "test_load.cbc", "test_overflow.cbc", "test_complex.cbc"
    ]

cleanupAllTestFiles :: IO ()
cleanupAllTestFiles = mapM_ cleanupTestFile testFiles

-- ==========================
-- Helpers pour parser le bytecode généré
-- ==========================

parseHeader :: BL.ByteString -> Maybe (Word32, Word16, Word8)
parseHeader bs =
    case runGetOrFail getHeaderData bs of
        Right (_, _, result) -> Just result
        Left _ -> Nothing
  where
    getHeaderData = do
        magic <- getWord32be
        version <- getWord16be
        flags <- getWord8
        skip 3
        return (magic, version, flags)

parseConstantPoolCount :: BL.ByteString -> Maybe Int
parseConstantPoolCount bs =
    case runGetOrFail (skip 10 >> fmap fromIntegral getWord32be) bs of
        Right (_, _, count) -> Just count
        Left _ -> Nothing

parseFunctionTableCount :: BL.ByteString -> Maybe Int
parseFunctionTableCount bs =
    case runGetOrFail parser bs of
        Right (_, _, count) -> Just count
        Left _ -> Nothing
  where
    parser = do
        skip 10
        constCount <- getWord32be
        skipConstantPool (fromIntegral constCount)
        fromIntegral <$> getWord32be

    skipConstantPool 0 = return ()
    skipConstantPool n = do
        _ <- getWord8
        len <- getWord32be
        skip (fromIntegral len)
        skipConstantPool (n - 1)

-- ==========================
-- Tests
-- ==========================

spec :: Spec
spec = do
    afterAll_ cleanupAllTestFiles $ do
        describe "AstToBin" $ do
            describe "Header Generation" $ do
                it "génère un header valide avec le magic number CBC\\0" $ do
                    let ast = IAProgram []
                    result <- parseBin ast "test_header.cbc"
                    result `shouldBe` Right ()

                    bytecode <- BS.readFile "test_header.cbc"
                    let header = parseHeader (BL.fromStrict bytecode)
                    header `shouldBe` Just (0x43424300, 0x0100, 0x00)

                it "génère un header de 10 bytes" $ do
                    let ast = IAProgram []
                    _ <- parseBin ast "test_header_size.cbc"
                    bytecode <- BS.readFile "test_header_size.cbc"
                    BS.length bytecode `shouldSatisfy` (>= 10)

        describe "Constant Pool" $ do
            it "génère un constant pool vide pour un programme vide" $ do
                let ast = IAProgram []
                _ <- parseBin ast "test_empty_pool.cbc"
                bytecode <- BS.readFile "test_empty_pool.cbc"
                let count = parseConstantPoolCount (BL.fromStrict bytecode)
                count `shouldBe` Just 0

            it "ajoute une string au constant pool" $ do
                let ast = IAProgram [IAMain [] [IAString "hello"]]
                _ <- parseBin ast "test_string_pool.cbc"
                bytecode <- BS.readFile "test_string_pool.cbc"
                let count = parseConstantPoolCount (BL.fromStrict bytecode)
                count `shouldSatisfy` maybe False (> 0)

            it "ajoute un symbole au constant pool" $ do
                let ast = IAProgram [IAMain [] [IADeclare "x" Nothing (IANumber 42)]]
                _ <- parseBin ast "test_symbol_pool.cbc"
                bytecode <- BS.readFile "test_symbol_pool.cbc"
                let count = parseConstantPoolCount (BL.fromStrict bytecode)
                count `shouldBe` Just 1

            it "déduplique les constantes identiques" $ do
                let ast = IAProgram [IAMain [] [IAString "test", IAString "test"]]
                _ <- parseBin ast "test_dedup.cbc"
                bytecode <- BS.readFile "test_dedup.cbc"
                let count = parseConstantPoolCount (BL.fromStrict bytecode)
                count `shouldBe` Just 1

        describe "Function Table" $ do
            it "génère une function table vide pour un programme sans fonction" $ do
                let ast = IAProgram [IAMain [] [IANumber 42]]
                _ <- parseBin ast "test_no_func.cbc"
                bytecode <- BS.readFile "test_no_func.cbc"
                let count = parseFunctionTableCount (BL.fromStrict bytecode)
                count `shouldBe` Just 0

            it "ajoute une fonction à la function table" $ do
                let ast = IAProgram [
                        IAFunctionDef "test" [] Nothing [IAReturn (IANumber 42)],
                        IAMain [] []
                        ]
                _ <- parseBin ast "test_one_func.cbc"
                bytecode <- BS.readFile "test_one_func.cbc"
                let count = parseFunctionTableCount (BL.fromStrict bytecode)
                count `shouldBe` Just 1

            it "gère plusieurs fonctions" $ do
                let ast = IAProgram [
                        IAFunctionDef "func1" [] Nothing [IAReturn (IANumber 1)],
                        IAFunctionDef "func2" [] Nothing [IAReturn (IANumber 2)],
                        IAMain [] []
                        ]
                _ <- parseBin ast "test_multi_func.cbc"
                bytecode <- BS.readFile "test_multi_func.cbc"
                let count = parseFunctionTableCount (BL.fromStrict bytecode)
                count `shouldBe` Just 2

        describe "Instructions Generation" $ do
            it "génère PUSH_INT pour un nombre" $ do
                let ast = IAProgram [IAMain [] [IANumber 42]]
                _ <- parseBin ast "test_push_int.cbc"
                bytecode <- BS.readFile "test_push_int.cbc"
                BS.elem 0x02 bytecode `shouldBe` True

            it "génère PUSH_BOOL pour un booléen" $ do
                let ast = IAProgram [IAMain [] [IABoolean True]]
                _ <- parseBin ast "test_push_bool.cbc"
                bytecode <- BS.readFile "test_push_bool.cbc"

                BS.elem 0x04 bytecode `shouldBe` True

            it "génère ADD pour une addition" $ do
                let ast = IAProgram [IAMain [] [IAInfix (IANumber 1) "+" (IANumber 2)]]
                _ <- parseBin ast "test_add.cbc"
                bytecode <- BS.readFile "test_add.cbc"

                BS.elem 0x10 bytecode `shouldBe` True

            it "génère PRINT pour afficher" $ do
                let ast = IAProgram [IAMain [] [IACall "afficher" [IANumber 42]]]
                _ <- parseBin ast "test_print.cbc"
                bytecode <- BS.readFile "test_print.cbc"

                BS.elem 0x80 bytecode `shouldBe` True

            it "se termine toujours par HALT" $ do
                let ast = IAProgram [IAMain [] [IANumber 42]]
                _ <- parseBin ast "test_halt.cbc"
                bytecode <- BS.readFile "test_halt.cbc"

                BS.last bytecode `shouldBe` 0xFF

        describe "Control Flow" $ do
            it "génère JMP_IF_FALSE pour un if" $ do
                let ast = IAProgram [IAMain [] [
                        IAIf (IABoolean True) [IANumber 1] Nothing
                        ]]
                _ <- parseBin ast "test_if.cbc"
                bytecode <- BS.readFile "test_if.cbc"

                BS.elem 0x62 bytecode `shouldBe` True

            it "génère JMP pour un if-else" $ do
                let ast = IAProgram [IAMain [] [
                        IAIf (IABoolean True) [IANumber 1] (Just [IANumber 2])
                        ]]
                _ <- parseBin ast "test_if_else.cbc"
                bytecode <- BS.readFile "test_if_else.cbc"

                BS.elem 0x60 bytecode `shouldBe` True
                BS.elem 0x62 bytecode `shouldBe` True

        describe "Functions" $ do
            it "génère CLOSURE pour une définition de fonction" $ do
                let ast = IAProgram [
                        IAFunctionDef "test" [] Nothing [IAReturn (IANumber 42)],
                        IAMain [] []
                        ]
                _ <- parseBin ast "test_closure.cbc"
                bytecode <- BS.readFile "test_closure.cbc"

                BS.elem 0x72 bytecode `shouldBe` True

            it "génère CALL pour un appel de fonction" $ do
                let ast = IAProgram [
                        IAFunctionDef "test" [] Nothing [IAReturn (IANumber 42)],
                        IAMain [] [IACall "test" []]
                        ]
                _ <- parseBin ast "test_call.cbc"
                bytecode <- BS.readFile "test_call.cbc"

                BS.elem 0x70 bytecode `shouldBe` True

            it "génère RETURN pour un retour" $ do
                let ast = IAProgram [
                        IAFunctionDef "test" [] Nothing [IAReturn (IANumber 42)]
                        ]
                _ <- parseBin ast "test_return.cbc"
                bytecode <- BS.readFile "test_return.cbc"

                BS.elem 0x71 bytecode `shouldBe` True

            it "génère LOAD_ARG pour les paramètres de fonction" $ do
                let ast = IAProgram [
                        IAFunctionDef "test" [("x", Nothing)] Nothing [IAReturn (IASymbol "x")],
                        IAMain [] []
                        ]
                _ <- parseBin ast "test_load_arg.cbc"
                bytecode <- BS.readFile "test_load_arg.cbc"

                BS.elem 0x73 bytecode `shouldBe` True

        describe "Variables" $ do
            it "génère DEFINE pour une déclaration" $ do
                let ast = IAProgram [IAMain [] [IADeclare "x" Nothing (IANumber 42)]]
                _ <- parseBin ast "test_define.cbc"
                bytecode <- BS.readFile "test_define.cbc"

                BS.elem 0x52 bytecode `shouldBe` True

            it "génère STORE pour une assignation" $ do
                let ast = IAProgram [IAMain [] [
                        IADeclare "x" Nothing (IANumber 0),
                        IAAssign "x" (IANumber 42)
                        ]]
                _ <- parseBin ast "test_store.cbc"
                bytecode <- BS.readFile "test_store.cbc"

                BS.elem 0x51 bytecode `shouldBe` True

            it "génère LOAD pour charger une variable" $ do
                let ast = IAProgram [IAMain [] [
                        IADeclare "x" Nothing (IANumber 42),
                        IASymbol "x"
                        ]]
                _ <- parseBin ast "test_load.cbc"
                bytecode <- BS.readFile "test_load.cbc"

                BS.elem 0x50 bytecode `shouldBe` True

        describe "Error Handling" $ do
            it "rejette un entier hors limites" $ do
                let ast = IAProgram [IAMain [] [IANumber 9999999999999]]
                    isLeft (Left _) = True
                    isLeft _ = False
                result <- parseBin ast "test_overflow.cbc"
                result `shouldSatisfy` isLeft

        describe "Complex Programs" $ do
            it "génère un bytecode valide pour un programme complexe" $ do
                let ast = IAProgram [
                        IAFunctionDef "factorial" [("n", Nothing)] Nothing [
                            IAIf (IAInfix (IASymbol "n") "<=" (IANumber 1))
                                [IAReturn (IANumber 1)]
                                (Just [IAReturn (IAInfix (IASymbol "n") "*"
                                    (IACall "factorial" [IAInfix (IASymbol "n") "-" (IANumber 1)]))])
                        ],
                        IAMain [] [
                            IACall "afficher" [IACall "factorial" [IANumber 5]]
                        ]
                        ]
                result <- parseBin ast "test_complex.cbc"
                result `shouldBe` Right ()

                bytecode <- BS.readFile "test_complex.cbc"

                BS.elem 0x72 bytecode `shouldBe` True
                BS.elem 0x70 bytecode `shouldBe` True
                BS.elem 0x62 bytecode `shouldBe` True
                BS.elem 0x71 bytecode `shouldBe` True
                BS.elem 0x80 bytecode `shouldBe` True
                BS.elem 0xFF bytecode `shouldBe` True
