from typing import Union
from vm_types import Value
from execution.state import VMState

def op_load(name: str, state: VMState) -> Union[VMState, str]:
    if name in state.env:
        state.stack.append(state.env[name])
    else:
        return f"Error: Variable '{name}' not defined"
    return state

def op_store(name: str, state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Store requires a value on the stack"
    
    val = state.stack[-1]
    
    val = state.stack.pop()
    
    if name in state.env:
        state.env[name] = val
    else:
        state.stack.append(val)
        return f"Error: Variable '{name}' not defined (use Define first)"
        
    return state

def op_define(name: str, state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Define requires a value on the stack"
    
    val = state.stack.pop()
    state.env[name] = val
    return state
