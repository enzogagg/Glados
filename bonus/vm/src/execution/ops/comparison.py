from typing import Union
from vm_types import Value, IntVal, FloatVal, BoolVal
from execution.state import VMState

def op_eq(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Eq requires two values on the stack"
    a = state.stack.pop()
    b = state.stack.pop()
    
    res = False
    if isinstance(a, IntVal) and isinstance(b, IntVal): res = (a.val == b.val)
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal): res = (a.val == b.val)
    elif isinstance(a, IntVal) and isinstance(b, FloatVal): res = (float(a.val) == b.val)
    elif isinstance(a, FloatVal) and isinstance(b, IntVal): res = (a.val == float(b.val))
    elif type(a) == type(b): res = (a == b)
    else: res = False
    
    state.stack.append(BoolVal(res))
    return state

def op_neq(state: VMState) -> Union[VMState, str]:
    res = op_eq(state)
    if isinstance(res, str): return res
    
    val = state.stack.pop()
    state.stack.append(BoolVal(not val.val))
    return state

def op_lt(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Lt requires two numeric values on the stack"
    a = state.stack.pop()
    b = state.stack.pop()
    
    res = False
    if isinstance(a, IntVal) and isinstance(b, IntVal): res = (b.val < a.val)
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal): res = (b.val < a.val)
    elif isinstance(a, IntVal) and isinstance(b, FloatVal): res = (b.val < float(a.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal): res = (float(b.val) < a.val)
    else:
        state.stack.append(b); state.stack.append(a)
        return "Error: Lt requires two numeric values on the stack"
        
    state.stack.append(BoolVal(res))
    return state

def op_gt(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Gt requires two numeric values on the stack"
    a = state.stack.pop()
    b = state.stack.pop()
    
    res = False
    if isinstance(a, IntVal) and isinstance(b, IntVal): res = (b.val > a.val)
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal): res = (b.val > a.val)
    elif isinstance(a, IntVal) and isinstance(b, FloatVal): res = (b.val > float(a.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal): res = (float(b.val) > a.val)
    else:
        state.stack.append(b); state.stack.append(a)
        return "Error: Gt requires two numeric values on the stack"
        
    state.stack.append(BoolVal(res))
    return state

def op_le(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Le requires two numeric values on the stack"
    a = state.stack.pop()
    b = state.stack.pop()
    
    res = False
    if isinstance(a, IntVal) and isinstance(b, IntVal): res = (b.val <= a.val)
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal): res = (b.val <= a.val)
    elif isinstance(a, IntVal) and isinstance(b, FloatVal): res = (b.val <= float(a.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal): res = (float(b.val) <= a.val)
    else:
        state.stack.append(b); state.stack.append(a)
        return "Error: Le requires two numeric values on the stack"
        
    state.stack.append(BoolVal(res))
    return state

def op_ge(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Ge requires two numeric values on the stack"
    a = state.stack.pop()
    b = state.stack.pop()
    
    res = False
    if isinstance(a, IntVal) and isinstance(b, IntVal): res = (b.val >= a.val)
    elif isinstance(a, FloatVal) and isinstance(b, FloatVal): res = (b.val >= a.val)
    elif isinstance(a, IntVal) and isinstance(b, FloatVal): res = (b.val >= float(a.val))
    elif isinstance(a, FloatVal) and isinstance(b, IntVal): res = (float(b.val) >= a.val)
    else:
        state.stack.append(b); state.stack.append(a)
        return "Error: Ge requires two numeric values on the stack"
        
    state.stack.append(BoolVal(res))
    return state

def op_and(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: And requires two boolean values on the stack"
    a = state.stack.pop()
    b = state.stack.pop()
    
    if isinstance(a, BoolVal) and isinstance(b, BoolVal):
        state.stack.append(BoolVal(b.val and a.val))
    else:
        state.stack.append(b); state.stack.append(a)
        return "Error: And requires two boolean values on the stack"
    return state

def op_or(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Or requires two boolean values on the stack"
    a = state.stack.pop()
    b = state.stack.pop()
    
    if isinstance(a, BoolVal) and isinstance(b, BoolVal):
        state.stack.append(BoolVal(b.val or a.val))
    else:
        state.stack.append(b); state.stack.append(a)
        return "Error: Or requires two boolean values on the stack"
    return state
    
def op_not(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Not requires one boolean value on the stack"
    a = state.stack.pop()
    
    if isinstance(a, BoolVal):
        state.stack.append(BoolVal(not a.val))
    else:
        state.stack.append(a)
        return "Error: Not requires one boolean value on the stack"
    return state
