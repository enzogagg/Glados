module Disassembler (disassemble) where

import Types
import Text.Printf (printf)

disassemble :: BytecodeFile -> IO ()
disassemble (BytecodeFile header consts funcs instrs) = do
    putStrLn "--- HEADER ---"
    printf "Magic: %x\n" (magic header)
    printf "Version: %x\n" (version header)
    printf "Flags: %x\n" (flags header)

    putStrLn "\n--- CONSTANT POOL ---"
    mapM_ printConst (zip [0..] consts)

    putStrLn "\n--- FUNCTION TABLE ---"
    mapM_ printFunc (zip [0..] funcs)

    putStrLn "\n--- INSTRUCTIONS ---"
    mapM_ printInstr (zip [0..] instrs)

printConst :: (Int, Value) -> IO ()
printConst (idx, val) = printf "%d: %s\n" idx (show val)

printFunc :: (Int, FunctionMeta) -> IO ()
printFunc (idx, meta) = printf "%d: FuncId=%d Address=%d Args=%d\n" idx (funcId meta) (funcAddress meta) (funcArgCount meta)

printInstr :: (Int, Instruction) -> IO ()
printInstr (idx, instr) = printf "%d: %s\n" idx (show instr)
