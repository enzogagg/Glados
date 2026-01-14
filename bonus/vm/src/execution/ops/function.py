from typing import Union
from vm_types import Value, FunctionVal
from execution.state import VMState

def op_call(func_idx: int, arg_count: int, state: VMState) -> Union[VMState, str]:

    target_func = None
    for f in state.functions:
        if f.index == func_idx:
            target_func = f
            break
            
    if target_func is None:
        return f"Error: Function index {func_idx} not found"
        
    if len(state.stack) < arg_count:
        return f"Error: Call requires {arg_count} arguments on the stack"
    
    args = []
    for _ in range(arg_count):
        args.append(state.stack.pop())
    
    ret_addr = state.ip
    
    state.call_stack.append((ret_addr, state.cur_args))
    state.cur_args = args
    state.ip = target_func.address - 1 
    
    return state

def op_return(state: VMState) -> Union[VMState, str]:
    if not state.call_stack:
        return "Error: Return with empty call stack"
    
    ret_addr, old_args = state.call_stack.pop()
    state.ip = ret_addr
    state.cur_args = old_args
    return state

def op_closure(func_idx: int, state: VMState) -> Union[VMState, str]:
    state.stack.append(FunctionVal(func_idx))
    return state

def op_load_arg(idx: int, state: VMState) -> Union[VMState, str]:
    if idx < 0 or idx >= len(state.cur_args):
        return f"Error: Argument index {idx} out of bounds"
    state.stack.append(state.cur_args[idx])
    return state
