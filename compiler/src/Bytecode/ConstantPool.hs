{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- Bytecode Constant Pool Generation
-}

module Bytecode.ConstantPool (
    putConstantEntry,
    generateConstantPool
) where

import Types
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<|>))

-- ==========================
-- Génération du Constant Pool
-- ==========================

type PutGen = Maybe Put

genInt :: ConstantEntry -> PutGen
genInt (ConstInt n) = Just $ do
    putWord8 (typeTagToByte TagInt)
    putWord32be 4
    putWord32be (fromIntegral n)
genInt _ = Nothing

genFloat :: ConstantEntry -> PutGen
genFloat (ConstFloat f) = Just $ do
    putWord8 (typeTagToByte TagFloat)
    putWord32be 4  -- Float32 = 4 bytes (pas 8)
    putFloatbe (realToFrac f)
genFloat _ = Nothing

genBool :: ConstantEntry -> PutGen
genBool (ConstBool b) = Just $ do
    putWord8 (typeTagToByte TagBool)
    putWord32be 1
    putWord8 (if b then 0x01 else 0x00)
genBool _ = Nothing

genChar :: ConstantEntry -> PutGen
genChar (ConstChar c) = Just $ do
    putWord8 (typeTagToByte TagChar)
    putWord32be 1
    putWord8 (fromIntegral $ fromEnum c)
genChar _ = Nothing

genString :: ConstantEntry -> PutGen
genString (ConstString s) = Just $ do
    putWord8 (typeTagToByte TagString)
    let bytes = map (fromIntegral . fromEnum) s
    putWord32be (fromIntegral $ length bytes)
    mapM_ putWord8 bytes
genString _ = Nothing

genList :: ConstantEntry -> PutGen
genList (ConstList entries) = Just $ do
    putWord8 (typeTagToByte TagList)
    let entriesBytes = BL.toStrict $ runPut $ mapM_ putConstantEntry entries
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes
genList _ = Nothing

genSymbol :: ConstantEntry -> PutGen
genSymbol (ConstSymbol s) = Just $ do
    putWord8 (typeTagToByte TagSymbol)
    let bytes = map (fromIntegral . fromEnum) s
    putWord32be (fromIntegral $ length bytes)
    mapM_ putWord8 bytes
genSymbol _ = Nothing

genNil :: ConstantEntry -> PutGen
genNil ConstNil = Just $ do
    putWord8 (typeTagToByte TagNil)
    putWord32be 0
genNil _ = Nothing

genTuple :: ConstantEntry -> PutGen
genTuple (ConstTuple entries) = Just $ do
    putWord8 (typeTagToByte TagTuple)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length entries)
            mapM_ putConstantEntry entries
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes
genTuple _ = Nothing

genArray :: ConstantEntry -> PutGen
genArray (ConstArray entries) = Just $ do
    putWord8 (typeTagToByte TagArray)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length entries)
            mapM_ putConstantEntry entries
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes
genArray _ = Nothing

genStruct :: ConstantEntry -> PutGen
genStruct (ConstStruct fields) = Just $ do
    putWord8 (typeTagToByte TagStruct)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length fields)
            mapM_ (\(name, value) -> do
                let nameBytes = map (fromIntegral . fromEnum) name
                putWord32be (fromIntegral $ length nameBytes)
                mapM_ putWord8 nameBytes
                putConstantEntry value) fields
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes
genStruct _ = Nothing

genMap :: ConstantEntry -> PutGen
genMap (ConstMap pairs) = Just $ do
    putWord8 (typeTagToByte TagMap)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length pairs)
            mapM_ (\(key, value) -> do
                putConstantEntry key
                putConstantEntry value) pairs
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes
genMap _ = Nothing

putConstantEntry :: ConstantEntry -> Put
putConstantEntry entry =
    case genInt entry
        <|> genFloat entry
        <|> genBool entry
        <|> genChar entry
        <|> genString entry
        <|> genList entry
        <|> genSymbol entry
        <|> genNil entry
        <|> genTuple entry
        <|> genArray entry
        <|> genStruct entry
        <|> genMap entry of
            Just result -> result
            Nothing -> error $ "Unsupported ConstantEntry: " ++ show entry

generateConstantPool :: [ConstantEntry] -> Put
generateConstantPool entries = do
    putWord32be (fromIntegral $ length entries)
    mapM_ putConstantEntry entries
