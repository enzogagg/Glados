from typing import Union, Tuple
from vm_types import Value, IntVal, FloatVal
from execution.state import VMState

def op_add(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Add requires two numeric values on the stack"
    
    a = state.stack.pop()
    b = state.stack.pop()
    
    if isinstance(a, IntVal) and isinstance(b, IntVal):
        state.stack.append(IntVal(a.val + b.val))
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(a.val + b.val))
    elif isinstance(a, IntVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(a.val + b.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal):
        state.stack.append(FloatVal(a.val + b.val))
    else:
        state.stack.append(b)
        state.stack.append(a)
        return "Error: Add requires two numeric values on the stack"
        
    return state

def op_sub(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Sub requires two numeric values on the stack"
    
    a = state.stack.pop()
    b = state.stack.pop()
    
    if isinstance(a, IntVal) and isinstance(b, IntVal):
        state.stack.append(IntVal(b.val - a.val))
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(b.val - a.val))
    elif isinstance(a, IntVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(b.val - a.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal):
        state.stack.append(FloatVal(b.val - a.val))
    else:
        state.stack.append(b)
        state.stack.append(a)
        return "Error: Sub requires two numeric values on the stack"
        
    return state

def op_mul(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Mul requires two numeric values on the stack"
    
    a = state.stack.pop()
    b = state.stack.pop()
    
    if isinstance(a, IntVal) and isinstance(b, IntVal):
        state.stack.append(IntVal(a.val * b.val))
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(a.val * b.val))
    elif isinstance(a, IntVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(a.val * b.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal):
        state.stack.append(FloatVal(a.val * b.val))
    else:
        state.stack.append(b)
        state.stack.append(a)
        return "Error: Mul requires two numeric values on the stack"
        
    return state

def op_div(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Div requires two numeric values on the stack"
    
    a = state.stack.pop()
    b = state.stack.pop()
    
    if (isinstance(a, IntVal) and a.val == 0) or (isinstance(a, FloatVal) and a.val == 0.0):
        state.stack.append(b)
        state.stack.append(a)
        return "Error: Division by zero"

    if isinstance(a, IntVal) and isinstance(b, IntVal):
        state.stack.append(IntVal(b.val // a.val))
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(b.val / a.val))
    elif isinstance(a, IntVal) and isinstance(b, FloatVal):
        state.stack.append(FloatVal(b.val / a.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal):
        state.stack.append(FloatVal(b.val / a.val))
    else:
        state.stack.append(b)
        state.stack.append(a)
        return "Error: Div requires two numeric values on the stack"
        
    return state

def op_mod(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Mod requires two numeric values of the same type on the stack"
    
    a = state.stack.pop()
    b = state.stack.pop()
    
    if isinstance(a, IntVal) and isinstance(b, IntVal):
        if a.val == 0:
            state.stack.append(b)
            state.stack.append(a)
            return "Error: Modulo by zero"
        state.stack.append(IntVal(b.val % a.val))
    else:
        state.stack.append(b)
        state.stack.append(a)
        return "Error: Mod requires two numeric values of the same type on the stack"
        
    return state

def op_neg(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Neg requires a numeric value on the stack"
    
    a = state.stack.pop()
    
    if isinstance(a, IntVal):
        state.stack.append(IntVal(-a.val))
    elif isinstance(a, FloatVal):
        state.stack.append(FloatVal(-a.val))
    else:
        state.stack.append(a)
        return "Error: Neg requires a numeric value on the stack"
        
    return state
