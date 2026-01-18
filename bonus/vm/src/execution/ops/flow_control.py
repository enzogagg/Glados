from typing import Union
from vm_types import Value, BoolVal
from execution.state import VMState

def op_jump(addr: int, state: VMState) -> Union[VMState, str]:
    state.ip = addr - 1
    return state

def op_jump_if_true(addr: int, state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: JumpIfTrue requires a value on the stack"
    
    val = state.stack.pop()
    if isinstance(val, BoolVal):
        if val.val:
            state.ip = addr - 1
    else:
        return "Error: JumpIfTrue requires a boolean on the stack"
        
    return state

def op_jump_if_false(addr: int, state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: JumpIfFalse requires a value on the stack"
    
    val = state.stack.pop()
    
    if isinstance(val, BoolVal):
        if not val.val:
            state.ip = addr - 1
    else:
        return "Error: JumpIfFalse requires a boolean on the stack"
        
    return state
