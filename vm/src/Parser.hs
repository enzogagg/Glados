module Parser where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word32)
import Control.Monad (replicateM)
import Data.Int (Int32)
import Types
import Utils

parseBytecode :: Get BytecodeFile
parseBytecode = do
    hdr <- parseHeader
    consts <- parseConstantPool
    funcs <- parseFunctionTable
    instrs <- parseInstructions consts
    return $ BytecodeFile hdr consts funcs instrs

parseHeader :: Get Header
parseHeader = do
    magicNum <- getWord32be
    ver <- getWord16be
    flg <- getWord8
    skip 3
    if magicNum /= 0x43424300
        then fail "Invalid Magic Number"
        else return $ Header (fromIntegral magicNum) (fromIntegral ver) (fromIntegral flg)

parseConstantPool :: Get [Value]
parseConstantPool = do
    count <- getWord32be
    replicateM (fromIntegral count) parseConstantEntry

parseConstantEntry :: Get Value
parseConstantEntry = do
    tag <- getWord8
    len <- getWord32be
    case tag of
        0x00 -> IntVal . fromIntegral <$> getInt32be
        0x01 -> FloatVal . word32ToFloat <$> getWord32be
        0x02 -> do
            val <- getWord8
            return $ BoolVal (val /= 0)
        0x03 -> CharVal . toEnum . fromIntegral <$> getWord8
        0x04 -> do
            str <- getByteString (fromIntegral len)
            return $ StringVal (map (toEnum . fromEnum) (BL.unpack (BL.fromStrict str)))
        0x05 -> do -- List
            -- TODO
            _ <- getByteString (fromIntegral len)
            return $ ListVal [] 
        0x06 -> do
            str <- getByteString (fromIntegral len)
            return $ SymbolVal (map (toEnum . fromEnum) (BL.unpack (BL.fromStrict str)))
        0x07 -> return NilVal
        0x08 -> FunctionVal . fromIntegral <$> getInt32be
        0x09 -> do -- Tuple
             _ <- getByteString (fromIntegral len)
             return $ TupleVal []
        0x0A -> do -- Array
             _ <- getByteString (fromIntegral len)
             return $ ArrayVal []
        0x0B -> do -- Struct
             _ <- getByteString (fromIntegral len)
             return $ StructVal []
        0x0C -> do -- Map
             _ <- getByteString (fromIntegral len)
             return $ MapVal []
        _ -> fail $ "Unknown constant type tag: " ++ show tag

parseFunctionTable :: Get [FunctionMeta]
parseFunctionTable = do
    count <- getWord32be
    replicateM (fromIntegral count) parseFunctionEntry

parseFunctionEntry :: Get FunctionMeta
parseFunctionEntry = do
    idx <- getWord32be
    addr <- getWord32be
    FunctionMeta (fromIntegral idx) (fromIntegral addr) . fromIntegral <$> getWord8

parseInstructions :: [Value] -> Get [Instruction]
parseInstructions pool = do
    len <- getWord32be
    bytes <- getLazyByteString (fromIntegral len)
    return $ runGet (parseInstructionLoop pool) bytes

parseInstructionLoop :: [Value] -> Get [Instruction]
parseInstructionLoop pool = do
    empty <- isEmpty
    if empty
        then return []
        else do
            instr <- parseInstruction pool
            rest <- parseInstructionLoop pool
            return (instr : rest)

parseInstruction :: [Value] -> Get Instruction
parseInstruction pool = do
    opcode <- getWord8
    case opcode of
        0x01 -> PushConst . fromIntegral <$> getInt32be
        0x02 -> PushInt . fromIntegral <$> getInt32be
        0x03 -> PushFloat . word32ToFloat <$> getWord32be
        0x04 -> PushBool . (/= 0) <$> getWord8
        0x05 -> PushString . getStringFromPool pool . fromIntegral <$> getInt32be
        0x06 -> return PushNil
        0x07 -> return Pop

        0x10 -> return Add
        0x11 -> return Sub
        0x12 -> return Mul
        0x13 -> return Div
        0x14 -> return Mod
        0x15 -> return Neg

        0x20 -> return Eq
        0x21 -> return Neq
        0x22 -> return Lt
        0x23 -> return Gt
        0x24 -> return Le
        0x25 -> return Ge
        0x26 -> return And
        0x27 -> return Or
        0x28 -> return Not

        0x30 -> return Cons
        0x31 -> return Head
        0x32 -> return Tail
        0x33 -> ListMake . fromIntegral <$> getInt32be
        0x34 -> return Len
        0x35 -> return IsEmpty
        0x36 -> return Nth
        0x37 -> return Insert
        0x38 -> return Remove
        0x39 -> return Contains
        0x3A -> return Append
        0x3B -> return Reverse

        0x50 -> Load . getStringFromPool pool . fromIntegral <$> getInt32be
        0x51 -> Store . getStringFromPool pool . fromIntegral <$> getInt32be
        0x52 -> Define . getStringFromPool pool . fromIntegral <$> getInt32be

        0x60 -> Jump . fromIntegral <$> getInt32be
        0x61 -> JumpIfTrue . fromIntegral <$> getInt32be
        0x62 -> JumpIfFalse . fromIntegral <$> getInt32be

        0x70 -> do
            fIdx <- getInt32be
            Call (fromIntegral fIdx) . fromIntegral <$> getWord8
        0x71 -> return Return
        0x72 -> Closure . fromIntegral <$> getInt32be
        0x73 -> LoadArg . fromIntegral <$> getInt32be

        0x80 -> return Print
        0x81 -> return Input

        0x90 -> MakeTuple . fromIntegral <$> getInt32be
        0x91 -> TupleGet . fromIntegral <$> getInt32be
        0x92 -> MakeArray . fromIntegral <$> getInt32be
        0x93 -> return ArrayGet
        0x94 -> return ArraySet
        0x95 -> MakeMap . fromIntegral <$> getInt32be
        0x96 -> return MapGet
        0x97 -> return MapSet
        0x98 -> MakeStruct . fromIntegral <$> getInt32be
        0x99 -> StructGet . getStringFromPool pool . fromIntegral <$> getInt32be
        0x9A -> StructSet . getStringFromPool pool . fromIntegral <$> getInt32be

        0xA0 -> return OpenFile
        0xA1 -> return ReadFile
        0xA2 -> return WriteFile
        0xA3 -> return CloseFile

        0xFF -> return Halt
        _ -> fail $ "Unknown Opcode: " ++ show opcode

getStringFromPool :: [Value] -> Int -> String
getStringFromPool pool idx
    | idx < length pool = case pool !! idx of
        StringVal s -> s
        _ -> "INVALID_TYPE"
    | otherwise = "INVALID_INDEX"
