from typing import Union, List, Tuple
from execution.state import VMState
from vm_types import Value, TupleVal, ArrayVal, MapVal, StructVal, IntVal, StringVal

def op_make_tuple(n: int, state: VMState) -> Union[VMState, str]:
    if n < 0:
        return "Error: Tuple size cannot be negative"
    if len(state.stack) < n:
        return "Error: Not enough values on the stack"
        
    values = []
    for _ in range(n):
        values.append(state.stack.pop())

    state.stack.append(TupleVal(values[::-1]))
    return state

def op_tuple_get(idx: int, state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Stack empty"
        
    val = state.stack.pop()
    if not isinstance(val, TupleVal):
        state.stack.append(val)
        return "Error: TupleGet expects a Tuple at the top of the stack"
        
    if 0 <= idx < len(val.val):
        state.stack.append(val.val[idx])
    else:
        state.stack.append(val)
        return f"Error: Index {idx} out of bounds for Tuple size {len(val.val)}"
        
    return state

def op_make_array(n: int, state: VMState) -> Union[VMState, str]:
    if n < 0:
        return "Error: Array size cannot be negative"
    if len(state.stack) < n:
        return "Error: Not enough values on the stack"
        
    values = []
    for _ in range(n):
        values.append(state.stack.pop())

    state.stack.append(ArrayVal(values[::-1]))
    return state

def op_array_get(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
         return "Error: Stack underflow for ArrayGet"
         
    idx_val = state.stack.pop()
    arr_val = state.stack.pop()
    
    if not isinstance(idx_val, IntVal) or not isinstance(arr_val, ArrayVal):
        state.stack.append(arr_val)
        state.stack.append(idx_val)
        return "Error: ArrayGet expects an Integer (index) and an Array on the stack"
        
    idx = idx_val.val
    values = arr_val.val
    
    if 0 <= idx < len(values):
        state.stack.append(values[idx])
    else:
        state.stack.append(arr_val)
        state.stack.append(idx_val)
        return f"Error: Index {idx} out of bounds for Array"
        
    return state

def op_array_set(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 3:
         return "Error: Stack underflow for ArraySet"
         
    new_val = state.stack.pop()
    idx_val = state.stack.pop()
    arr_val = state.stack.pop()
    
    if not isinstance(idx_val, IntVal) or not isinstance(arr_val, ArrayVal):
        state.stack.append(arr_val)
        state.stack.append(idx_val)
        state.stack.append(new_val)
        return "Error: ArraySet expects Value, Integer (index), and Array on stack"
        
    idx = idx_val.val
    values = arr_val.val
    
    if 0 <= idx < len(values):
        new_list = list(values)
        new_list[idx] = new_val
        state.stack.append(ArrayVal(new_list))
    else:
        state.stack.append(arr_val)
        state.stack.append(idx_val)
        state.stack.append(new_val)
        return f"Error: Index {idx} out of bounds for Array"
        
    return state

def op_make_map(n: int, state: VMState) -> Union[VMState, str]:
    if n < 0:
        return "Error: Map size cannot be negative"
    if len(state.stack) < n * 2:
        return "Error: Not enough values on the stack (need (k,v) pairs)"

    pairs = []
    
    for _ in range(n):
        k = state.stack.pop()
        v = state.stack.pop()
        pairs.append( (k, v) )
    
    state.stack.append(MapVal(pairs))
    return state

def op_map_get(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: Stack underflow for MapGet"
        
    key = state.stack.pop()
    map_val = state.stack.pop()
    
    if not isinstance(map_val, MapVal):
        state.stack.append(map_val)
        state.stack.append(key)
        return "Error: MapGet expects Key and Map on stack"
        
    found = None
    for k, v in map_val.val:
        if k == key:
            found = v
            break
            
    if found:
        state.stack.append(found)
    else:
        state.stack.append(map_val)
        state.stack.append(key)
        return f"Error: Key not found in Map: {key}"
        
    return state

def op_map_set(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 3:
        return "Error: Stack underflow for MapSet"
        
    val = state.stack.pop()
    key = state.stack.pop()
    map_val = state.stack.pop()
    
    if not isinstance(map_val, MapVal):
        state.stack.append(map_val)
        state.stack.append(key)
        state.stack.append(val)
        return "Error: MapSet expects Value, Key, and Map on stack"
    
    old_pairs = map_val.val
    new_pairs = [(key, val)]
    
    for k, v in old_pairs:
        if k != key:
            new_pairs.append((k, v))
            
    state.stack.append(MapVal(new_pairs))
    return state

def op_make_struct(n: int, state: VMState) -> Union[VMState, str]:
    if n < 0:
        return "Error: Struct size cannot be negative"
        
    required = n * 2
    if len(state.stack) < required:
        return "Error: Not enough values for Struct (need key(string)-value pairs)"
    
    pairs = []
    popped = []
    
    try:
        err = None
        for _ in range(n):
            k = state.stack.pop(); popped.append(k)
            v = state.stack.pop(); popped.append(v)
            
            if not isinstance(k, StringVal):
                err = "Error: Struct keys must be Strings"
                break
            
            pairs.append( (k.val, v) )
    except IndexError:
        err = "Error: Not enough values"
        
    if err:
        for p in reversed(popped):
            state.stack.append(p)
        return err

    state.stack.append(StructVal(pairs))
    return state

def op_struct_get(name: str, state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Stack empty"
        
    struct_val = state.stack.pop()
    
    if not isinstance(struct_val, StructVal):
        state.stack.append(struct_val)
        return "Error: StructGet expects a Struct at the top of the stack"
    
    found = None
    for k, v in struct_val.val:
        if k == name:
            found = v
            break
            
    if found:
        state.stack.append(found)
    else:
        state.stack.append(struct_val)
        return f"Error: Field not found in Struct: {name}"
        
    return state

def op_struct_set(name: str, state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: Stack empty"
        
    if len(state.stack) < 2:
         return "Error: StructSet expects Value and Struct at the top of the stack"
         
    val = state.stack.pop()
    struct_val = state.stack.pop()
    
    if not isinstance(struct_val, StructVal):
        state.stack.append(struct_val)
        state.stack.append(val)
        return "Error: StructSet expects Value and Struct at the top of the stack"
        
    old_fields = struct_val.val
    new_fields = [(name, val)]
    
    for k, v in old_fields:
        if k != name:
            new_fields.append((k, v))
            
    state.stack.append(StructVal(new_fields))
    return state
