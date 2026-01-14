from typing import Union
from vm_types import Value, IntVal, FloatVal, BoolVal, StringVal, NilVal
from execution.state import VMState

def op_push_const(val: Value, state: VMState) -> Union[VMState, str]:
    state.stack.append(val)
    return state

def op_push_int(val: int, state: VMState) -> Union[VMState, str]:
    state.stack.append(IntVal(val))
    return state

def op_push_float(val: float, state: VMState) -> Union[VMState, str]:
    state.stack.append(FloatVal(val))
    return state

def op_push_bool(val: bool, state: VMState) -> Union[VMState, str]:
    state.stack.append(BoolVal(val))
    return state

def op_push_string(val: str, state: VMState) -> Union[VMState, str]:
    state.stack.append(StringVal(val))
    return state

def op_push_nil(state: VMState) -> Union[VMState, str]:
    state.stack.append(NilVal())
    return state

def op_pop(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Pop on empty stack"
    state.stack.pop()
    return state
