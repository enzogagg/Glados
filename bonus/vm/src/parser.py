import struct
import io
from typing import List
from vm_types import (
    Header, FunctionMeta, BytecodeFile,
    Value, IntVal, FloatVal, BoolVal, CharVal, StringVal, ListVal, SymbolVal, NilVal, 
    FunctionVal, TupleVal, ArrayVal, StructVal, MapVal,
    Instruction, PushConst, PushInt, PushFloat, PushBool, PushString, PushNil, Pop,
    Add, Sub, Mul, Div, Mod, Neg,
    Eq, Neq, Lt, Gt, Le, Ge, And, Or, Not,
    Cons, Head, Tail, ListMake, Len, IsEmpty, Nth, Insert, Remove, Contains, Append, Reverse,
    MakeSymbol, Quote, Eval,
    Load, Store, Define,
    Jump, JumpIfTrue, JumpIfFalse,
    Call, Return, Closure, LoadArg,
    Print, Input,
    MakeTuple, TupleGet, MakeArray, ArrayGet, ArraySet,
    MakeMap, MapGet, MapSet, MakeStruct, StructGet, StructSet,
    OpenFile, ReadFile, WriteFile, CloseFile, Halt
)

class Parser:
    def __init__(self, data: bytes):
        self.reader = io.BytesIO(data)

    def read_uint8(self) -> int:
        return struct.unpack('>B', self.reader.read(1))[0]

    def read_uint16(self) -> int:
        return struct.unpack('>H', self.reader.read(2))[0]

    def read_uint32(self) -> int:
        return struct.unpack('>I', self.reader.read(4))[0]

    def read_int32(self) -> int:
        return struct.unpack('>i', self.reader.read(4))[0]
    
    def read_float(self) -> float:
        return struct.unpack('>f', self.reader.read(4))[0]

    def read_bytes(self, n: int) -> bytes:
        return self.reader.read(n)
        
    def skip(self, n: int):
        self.reader.seek(n, 1)

    def parse_header(self) -> Header:
        magic_num = self.read_uint32()
        ver = self.read_uint16()
        flg = self.read_uint8()
        self.skip(3)
        
        if magic_num != 0x43424300:
            raise ValueError("Invalid Magic Number")
        
        return Header(magic_num, ver, flg)

    def parse_constant_pool(self) -> List[Value]:
        count = self.read_uint32()
        return [self.parse_constant_entry() for _ in range(count)]

    def parse_constant_entry(self) -> Value:
        tag = self.read_uint8()
        length = self.read_uint32()
        
        if tag == 0x00:
            return IntVal(self.read_int32())
        elif tag == 0x01:
            return FloatVal(self.read_float())
        elif tag == 0x02:
            val = self.read_uint8()
            return BoolVal(val != 0)
        elif tag == 0x03:
            return CharVal(chr(self.read_uint8()))
        elif tag == 0x04:
            s_bytes = self.read_bytes(length)
            return StringVal(s_bytes.decode('utf-8'))
        elif tag == 0x05:
            self.read_bytes(length)
            return ListVal([])
        elif tag == 0x06:
            s_bytes = self.read_bytes(length)
            return SymbolVal(s_bytes.decode('utf-8'))
        elif tag == 0x07:
            return NilVal()
        elif tag == 0x08:
            return FunctionVal(self.read_int32())
        elif tag == 0x09:
            self.read_bytes(length)
            return TupleVal([])
        elif tag == 0x0A:
            self.read_bytes(length)
            return ArrayVal([])
        elif tag == 0x0B:
            self.read_bytes(length)
            return StructVal([])
        elif tag == 0x0C:
            self.read_bytes(length)
            return MapVal([])
        else:
            raise ValueError(f"Unknown constant type tag: {tag}")

    def parse_function_table(self) -> List[FunctionMeta]:
        count = self.read_uint32()
        return [self.parse_function_entry() for _ in range(count)]

    def parse_function_entry(self) -> FunctionMeta:
        idx = self.read_uint32()
        addr = self.read_uint32()
        arg_count = self.read_uint8()
        return FunctionMeta(idx, addr, arg_count)

    def parse_instructions(self, pool: List[Value]) -> List[Instruction]:
        length = self.read_uint32()
        instr_data = self.read_bytes(length)
        sub_parser = Parser(instr_data)
        instrs = []
        
        # Read until EOF in the sub-parser
        while True:
            current = sub_parser.reader.tell()
            sub_parser.reader.seek(0, 2)
            end = sub_parser.reader.tell()
            sub_parser.reader.seek(current)
            if current >= end:
                break
            
            instrs.append(sub_parser.parse_instruction(pool))
            
        return instrs

    def parse_instruction(self, pool: List[Value]) -> Instruction:
        def get_string(idx):
            if idx < len(pool):
                v = pool[idx]
                if isinstance(v, StringVal):
                    return v.val
                return "INVALID_TYPE"
            return "INVALID_INDEX"

        opcode = self.read_uint8()
        
        if opcode == 0x01: return PushConst(self.read_int32())
        elif opcode == 0x02: return PushInt(self.read_int32())
        elif opcode == 0x03: return PushFloat(self.read_float())
        elif opcode == 0x04: return PushBool(self.read_uint8() != 0)
        elif opcode == 0x05: return PushString(get_string(self.read_int32()))
        elif opcode == 0x06: return PushNil()
        elif opcode == 0x07: return Pop()

        elif opcode == 0x10: return Add()
        elif opcode == 0x11: return Sub()
        elif opcode == 0x12: return Mul()
        elif opcode == 0x13: return Div()
        elif opcode == 0x14: return Mod()
        elif opcode == 0x15: return Neg()

        elif opcode == 0x20: return Eq()
        elif opcode == 0x21: return Neq()
        elif opcode == 0x22: return Lt()
        elif opcode == 0x23: return Gt()
        elif opcode == 0x24: return Le()
        elif opcode == 0x25: return Ge()
        elif opcode == 0x26: return And()
        elif opcode == 0x27: return Or()
        elif opcode == 0x28: return Not()

        elif opcode == 0x30: return Cons()
        elif opcode == 0x31: return Head()
        elif opcode == 0x32: return Tail()
        elif opcode == 0x33: return ListMake(self.read_int32())
        elif opcode == 0x34: return Len()
        elif opcode == 0x35: return IsEmpty()
        elif opcode == 0x36: return Nth()
        elif opcode == 0x37: return Insert()
        elif opcode == 0x38: return Remove()
        elif opcode == 0x39: return Contains()
        elif opcode == 0x3A: return Append()
        elif opcode == 0x3B: return Reverse()

        elif opcode == 0x40: return MakeSymbol()
        elif opcode == 0x41: return Quote()
        elif opcode == 0x42: return Eval()

        elif opcode == 0x50: return Load(get_string(self.read_int32()))
        elif opcode == 0x51: return Store(get_string(self.read_int32()))
        elif opcode == 0x52: return Define(get_string(self.read_int32()))

        elif opcode == 0x60: return Jump(self.read_int32())
        elif opcode == 0x61: return JumpIfTrue(self.read_int32())
        elif opcode == 0x62: return JumpIfFalse(self.read_int32())

        elif opcode == 0x70:
            f_idx = self.read_int32()
            arg_c = self.read_uint8()
            return Call(f_idx, arg_c)
        elif opcode == 0x71: return Return()
        elif opcode == 0x72: return Closure(self.read_int32())
        elif opcode == 0x73: return LoadArg(self.read_int32())

        elif opcode == 0x80: return Print()
        elif opcode == 0x81: return Input()

        elif opcode == 0x90: return MakeTuple(self.read_int32())
        elif opcode == 0x91: return TupleGet(self.read_int32())
        elif opcode == 0x92: return MakeArray(self.read_int32())
        elif opcode == 0x93: return ArrayGet()
        elif opcode == 0x94: return ArraySet()
        elif opcode == 0x95: return MakeMap(self.read_int32())
        elif opcode == 0x96: return MapGet()
        elif opcode == 0x97: return MapSet()
        elif opcode == 0x98: return MakeStruct(self.read_int32())
        elif opcode == 0x99: return StructGet(get_string(self.read_int32()))
        elif opcode == 0x9A: return StructSet(get_string(self.read_int32()))

        elif opcode == 0xA0: return OpenFile()
        elif opcode == 0xA1: return ReadFile()
        elif opcode == 0xA2: return WriteFile()
        elif opcode == 0xA3: return CloseFile()

        elif opcode == 0xFF: return Halt()
        
        else:
            raise ValueError(f"Unknown Opcode: {opcode}")

    def parse_bytecode(self) -> BytecodeFile:
        hdr = self.parse_header()
        consts = self.parse_constant_pool()
        funcs = self.parse_function_table()
        instrs = self.parse_instructions(consts)
        return BytecodeFile(hdr, consts, funcs, instrs)

def parse_file(filename: str) -> BytecodeFile:
    with open(filename, 'rb') as f:
        data = f.read()
    parser = Parser(data)
    return parser.parse_bytecode()
