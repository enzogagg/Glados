from typing import Union
from execution.state import VMState
from vm_types import Value, ListVal, NilVal, IntVal, BoolVal

def op_cons(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Cons requires two values on the stack"
    
    v1 = state.stack.pop()
    v2 = state.stack.pop()
    
    if isinstance(v2, ListVal):
        state.stack.append(ListVal([v1] + v2.val))
    elif isinstance(v2, NilVal):
        state.stack.append(ListVal([v1]))
    else:
        state.stack.append(v2)
        state.stack.append(v1)
        return "Error: Cons requires second value to be a list"
    return state

def op_head(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Head requires a value on the stack"
        
    v = state.stack.pop()
    
    if isinstance(v, ListVal):
        if not v.val:
            state.stack.append(v)
            return "Error: Head on empty list"
        state.stack.append(v.val[0])
    else:
        state.stack.append(v)
        return "Error: Head requires a list on the stack"
    return state

def op_tail(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Tail requires a value on the stack"
        
    v = state.stack.pop()
    
    if isinstance(v, ListVal):
        if not v.val:
            state.stack.append(v)
            return "Error: Tail on empty list"
        state.stack.append(ListVal(v.val[1:]))
    else:
        state.stack.append(v)
        return "Error: Tail requires a list on the stack"
    return state

def op_list_make(n: int, state: VMState) -> Union[VMState, str]:
    if len(state.stack) < n:
        return "Error: Not enough values on the stack to create list"
    
    vals = []
    for _ in range(n):
        vals.append(state.stack.pop())
    
    state.stack.append(ListVal(vals[::-1]))
    return state

def op_len(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Len requires a value on the stack"
        
    v = state.stack.pop()
    if isinstance(v, ListVal):
        state.stack.append(IntVal(len(v.val)))
    else:
        state.stack.append(v)
        return "Error: Len requires a list on the stack"
    return state

def op_is_empty(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: IsEmpty requires a value on the stack"
        
    v = state.stack.pop()
    if isinstance(v, ListVal):
        state.stack.append(BoolVal(len(v.val) == 0))
    else:
        state.stack.append(v)
        return "Error: IsEmpty requires a list on the stack"
    return state

def op_nth(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Nth requires an integer and a list on the stack"
    
    n_val = state.stack.pop()
    lst_val = state.stack.pop()
    
    if not isinstance(n_val, IntVal) or not isinstance(lst_val, ListVal):
        state.stack.append(lst_val); state.stack.append(n_val)
        return "Error: Nth requires an integer and a list on the stack"
        
    n = n_val.val
    lst = lst_val.val
    if n < 0 or n >= len(lst):
        state.stack.append(lst_val); state.stack.append(n_val)
        return "Error: Nth index out of bounds"
        
    state.stack.append(lst[n])
    return state

def op_insert(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 3:
        return "Error: Insert requires a value, an integer, and a list on the stack"
        
    v = state.stack.pop()
    n_val = state.stack.pop()
    lst_val = state.stack.pop()
    
    if not isinstance(n_val, IntVal) or not isinstance(lst_val, ListVal):
        state.stack.append(lst_val); state.stack.append(n_val); state.stack.append(v)
        return "Error: Insert requires a value, an integer, and a list on the stack"
        
    n = n_val.val
    lst = lst_val.val
    
    if n < 0 or n > len(lst):
        state.stack.append(lst_val); state.stack.append(n_val); state.stack.append(v)
        return "Error: Insert index out of bounds"
        
    new_lst = lst[:n] + [v] + lst[n:]
    state.stack.append(ListVal(new_lst))
    return state

def op_remove(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Remove requires an integer and a list on the stack"
        
    n_val = state.stack.pop()
    lst_val = state.stack.pop()
    
    if not isinstance(n_val, IntVal) or not isinstance(lst_val, ListVal):
        state.stack.append(lst_val); state.stack.append(n_val)
        return "Error: Remove requires an integer and a list on the stack"
        
    n = n_val.val
    lst = lst_val.val
    
    if n < 0 or n >= len(lst):
        state.stack.append(lst_val); state.stack.append(n_val)
        return "Error: Remove index out of bounds"
        
    new_lst = lst[:n] + lst[n+1:]
    state.stack.append(ListVal(new_lst))
    return state

def op_contains(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Contains requires a value and a list on the stack"
        
    v = state.stack.pop()
    lst_val = state.stack.pop()
    
    if not isinstance(lst_val, ListVal):
        state.stack.append(lst_val); state.stack.append(v)
        return "Error: Contains requires a value and a list on the stack"
        
    found = v in lst_val.val
    state.stack.append(BoolVal(found))
    return state

def op_append(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Append requires two lists on the stack"
        
    l2_val = state.stack.pop()
    l1_val = state.stack.pop()
    
    if not isinstance(l1_val, ListVal) or not isinstance(l2_val, ListVal):
        state.stack.append(l1_val); state.stack.append(l2_val)
        return "Error: Append requires two lists on the stack"
        
    state.stack.append(ListVal(l1_val.val + l2_val.val))
    return state

def op_reverse(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Reverse requires a list on the stack"
        
    v = state.stack.pop()
    if not isinstance(v, ListVal):
        state.stack.append(v)
        return "Error: Reverse requires a list on the stack"
        
    state.stack.append(ListVal(v.val[::-1]))
    return state
