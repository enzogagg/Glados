from dataclasses import dataclass
from typing import List, Tuple, Any

# --- Values ---

class Value:
    def __str__(self):
        return self.__repr__()

@dataclass
class IntVal(Value):
    val: int
    def __str__(self): return str(self.val)

@dataclass
class FloatVal(Value):
    val: float
    def __str__(self): return str(self.val)

@dataclass
class BoolVal(Value):
    val: bool
    def __str__(self): return "#t" if self.val else "#f"

@dataclass
class CharVal(Value):
    val: str
    def __str__(self): return f"'{self.val}'" # Haskell show Char puts quotes

@dataclass
class StringVal(Value):
    val: str
    def __str__(self):
        return f'"{self.val}"'

@dataclass
class ListVal(Value):
    val: List[Value]
    def __str__(self):
        return "(" + " ".join(str(v) for v in self.val) + ")"

@dataclass
class SymbolVal(Value):
    val: str
    def __str__(self): return self.val

class NilVal(Value):
    def __str__(self): return "nil"
    def __repr__(self): return "NilVal()"

@dataclass
class FunctionVal(Value):
    val: int
    def __str__(self): return f"<function {self.val}>"

@dataclass
class TupleVal(Value):
    val: List[Value]
    def __str__(self):
        return "{" + " ".join(str(v) for v in self.val) + "}"

@dataclass
class ArrayVal(Value):
    val: List[Value]
    def __str__(self):
        return "[" + " ".join(str(v) for v in self.val) + "]"

@dataclass
class StructVal(Value):
    val: List[Value]
    def __str__(self): return "<struct>"

@dataclass
class MapVal(Value):
    val: List[Value]
    def __str__(self): return "<map>"

@dataclass
class FileVal(Value):
    val: str
    def __str__(self): return f"<file {self.val}>"

# --- Instructions ---

class Instruction:
    pass

@dataclass
class PushConst(Instruction):
    val: int
    def __str__(self): return f"PushConst {self.val}"

@dataclass
class PushInt(Instruction):
    val: int
    def __str__(self): return f"PushInt {self.val}"

@dataclass
class PushFloat(Instruction):
    val: float
    def __str__(self): return f"PushFloat {self.val}"

@dataclass
class PushBool(Instruction):
    val: bool
    def __str__(self): return f"PushBool {self.val}"

@dataclass
class PushString(Instruction):
    val: str
    def __str__(self): return f"PushString {self.val!r}"

class PushNil(Instruction):
    def __str__(self): return "PushNil"
class Pop(Instruction):
    def __str__(self): return "Pop"

class Add(Instruction):
    def __str__(self): return "Add"
class Sub(Instruction):
    def __str__(self): return "Sub"
class Mul(Instruction):
    def __str__(self): return "Mul"
class Div(Instruction):
    def __str__(self): return "Div"
class Mod(Instruction):
    def __str__(self): return "Mod"
class Neg(Instruction):
    def __str__(self): return "Neg"

class Eq(Instruction):
    def __str__(self): return "Eq"
class Neq(Instruction):
    def __str__(self): return "Neq"
class Lt(Instruction):
    def __str__(self): return "Lt"
class Gt(Instruction):
    def __str__(self): return "Gt"
class Le(Instruction):
    def __str__(self): return "Le"
class Ge(Instruction):
    def __str__(self): return "Ge"
class And(Instruction):
    def __str__(self): return "And"
class Or(Instruction):
    def __str__(self): return "Or"
class Not(Instruction):
    def __str__(self): return "Not"

class Cons(Instruction):
    def __str__(self): return "Cons"
class Head(Instruction):
    def __str__(self): return "Head"
class Tail(Instruction):
    def __str__(self): return "Tail"

@dataclass
class ListMake(Instruction):
    val: int
    def __str__(self): return f"ListMake {self.val}"

class Len(Instruction):
    def __str__(self): return "Len"
class IsEmpty(Instruction):
    def __str__(self): return "IsEmpty"
class Nth(Instruction):
    def __str__(self): return "Nth"
class Insert(Instruction):
    def __str__(self): return "Insert"
class Remove(Instruction):
    def __str__(self): return "Remove"
class Contains(Instruction):
    def __str__(self): return "Contains"
class Append(Instruction):
    def __str__(self): return "Append"
class Reverse(Instruction):
    def __str__(self): return "Reverse"

class MakeSymbol(Instruction):
    def __str__(self): return "MakeSymbol"
class Quote(Instruction):
    def __str__(self): return "Quote"
class Eval(Instruction):
    def __str__(self): return "Eval"

@dataclass
class Load(Instruction):
    val: str
    def __str__(self): return f"Load {self.val!r}"

@dataclass
class Store(Instruction):
    val: str
    def __str__(self): return f"Store {self.val!r}"

@dataclass
class Define(Instruction):
    val: str
    def __str__(self): return f"Define {self.val!r}"

@dataclass
class Jump(Instruction):
    val: int
    def __str__(self): return f"Jump {self.val}"

@dataclass
class JumpIfTrue(Instruction):
    val: int
    def __str__(self): return f"JumpIfTrue {self.val}"

@dataclass
class JumpIfFalse(Instruction):
    val: int
    def __str__(self): return f"JumpIfFalse {self.val}"

@dataclass
class Call(Instruction):
    func_idx: int
    arg_count: int
    def __str__(self): return f"Call {self.func_idx} {self.arg_count}"

class Return(Instruction):
    def __str__(self): return "Return"

@dataclass
class Closure(Instruction):
    val: int
    def __str__(self): return f"Closure {self.val}"

@dataclass
class LoadArg(Instruction):
    val: int
    def __str__(self): return f"LoadArg {self.val}"

class Print(Instruction):
    def __str__(self): return "Print"
class Input(Instruction):
    def __str__(self): return "Input"

@dataclass
class MakeTuple(Instruction):
    val: int
    def __str__(self): return f"MakeTuple {self.val}"

@dataclass
class TupleGet(Instruction):
    val: int
    def __str__(self): return f"TupleGet {self.val}"

@dataclass
class MakeArray(Instruction):
    val: int
    def __str__(self): return f"MakeArray {self.val}"

class ArrayGet(Instruction):
    def __str__(self): return "ArrayGet"
class ArraySet(Instruction):
    def __str__(self): return "ArraySet"

@dataclass
class MakeMap(Instruction):
    val: int
    def __str__(self): return f"MakeMap {self.val}"

class MapGet(Instruction):
    def __str__(self): return "MapGet"
class MapSet(Instruction):
    def __str__(self): return "MapSet"

@dataclass
class MakeStruct(Instruction):
    val: int
    def __str__(self): return f"MakeStruct {self.val}"

@dataclass
class StructGet(Instruction):
    name: str
    def __str__(self): return f"StructGet {self.name!r}"

@dataclass
class StructSet(Instruction):
    name: str
    def __str__(self): return f"StructSet {self.name!r}"

class OpenFile(Instruction):
    def __str__(self): return "OpenFile"
class ReadFile(Instruction):
    def __str__(self): return "ReadFile"
class WriteFile(Instruction):
    def __str__(self): return "WriteFile"
class CloseFile(Instruction):
    def __str__(self): return "CloseFile"
class Halt(Instruction):
    def __str__(self): return "Halt"


# --- Meta ---

@dataclass
class Header:
    magic: int
    version: int
    flags: int

@dataclass
class FunctionMeta:
    index: int
    address: int
    arg_count: int

@dataclass
class BytecodeFile:
    header: Header
    pool: List[Value]
    functions: List[FunctionMeta]
    instructions: List[Instruction]
