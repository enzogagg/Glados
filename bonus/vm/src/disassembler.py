from vm_types import BytecodeFile, Value, FunctionMeta, Instruction

def disassemble(bytecode: BytecodeFile):
    print("--- HEADER ---")
    print(f"Magic: {bytecode.header.magic:x}")
    print(f"Version: {bytecode.header.version:x}")
    print(f"Flags: {bytecode.header.flags:x}")

    print("\n--- CONSTANT POOL ---")
    for idx, val in enumerate(bytecode.pool):
        print(f"{idx}: {val}")

    print("\n--- FUNCTION TABLE ---")
    for idx, func in enumerate(bytecode.functions):
        print(f"{idx}: FuncId={func.index} Address={func.address} Args={func.arg_count}")

    print("\n--- INSTRUCTIONS ---")
    for idx, instr in enumerate(bytecode.instructions):
        print(f"{idx}: {instr}")
