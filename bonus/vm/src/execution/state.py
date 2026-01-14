from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple
from vm_types import Value, Instruction, FunctionMeta, IntVal, StringVal, ListVal

@dataclass
class VMState:
    stack: List[Value] = field(default_factory=list)
    env: Dict[str, Value] = field(default_factory=dict)
    ip: int = 0
    call_stack: List[Tuple[int, List[Value]]] = field(default_factory=list) # (return_ip, previous_args)
    cur_args: List[Value] = field(default_factory=list)
    is_running: bool = False
    
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[Value] = field(default_factory=list)
    functions: List[FunctionMeta] = field(default_factory=list)

def new_vm_state(insts: List[Instruction], consts: List[Value], funcs: List[FunctionMeta], args: List[str]) -> VMState:
    initial_env = {
        "argc": IntVal(len(args)),
        "argv": ListVal([StringVal(arg) for arg in args])
    }
    
    return VMState(
        stack=[],
        env=initial_env,
        ip=0,
        call_stack=[],
        cur_args=[],
        is_running=False,
        instructions=insts,
        constants=consts,
        functions=funcs
    )
