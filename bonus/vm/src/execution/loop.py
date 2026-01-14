from vm_types import *
from execution.state import VMState
from execution.ops.arithmetic import op_add, op_sub, op_mul, op_div, op_mod, op_neg
from execution.ops.asset import (op_push_const, op_push_int, op_push_float, op_push_bool, 
                                 op_push_string, op_push_nil, op_pop)
from execution.ops.comparison import (op_eq, op_neq, op_lt, op_gt, op_le, op_ge, 
                                      op_and, op_or, op_not)
from execution.ops.flow_control import op_jump, op_jump_if_true, op_jump_if_false
from execution.ops.variable import op_load, op_store, op_define
from execution.ops.function import op_call, op_return, op_closure, op_load_arg
from execution.ops.input import op_input
from execution.ops.lists import (op_cons, op_head, op_tail, op_list_make, op_len, 
                                 op_is_empty, op_nth, op_insert, op_remove, 
                                 op_contains, op_append, op_reverse)
from execution.ops.complex import (op_make_tuple, op_tuple_get, op_make_array, 
                                   op_array_get, op_array_set, op_make_map, 
                                   op_map_get, op_map_set, op_make_struct, 
                                   op_struct_get, op_struct_set)
from execution.ops.file import op_open_file, op_read_file, op_write_file, op_close_file

def exec_loop(state: VMState):
    state.is_running = True
    
    while state.is_running:
        if state.ip < 0 or state.ip >= len(state.instructions):
            print("Error: Instruction pointer out of bounds")
            break
            
        instr = state.instructions[state.ip]
        
        res = None
        
        if isinstance(instr, Halt):
            state.is_running = False
            return

        elif isinstance(instr, PushConst):
            if 0 <= instr.val < len(state.constants):
                res = op_push_const(state.constants[instr.val], state)
            else: res = f"Error: Constant index out of bounds: {instr.val}"
        elif isinstance(instr, PushInt): res = op_push_int(instr.val, state)
        elif isinstance(instr, PushFloat): res = op_push_float(instr.val, state)
        elif isinstance(instr, PushBool): res = op_push_bool(instr.val, state)
        elif isinstance(instr, PushString): res = op_push_string(instr.val, state)
        elif isinstance(instr, PushNil): res = op_push_nil(state)
        elif isinstance(instr, Pop): res = op_pop(state)

        elif isinstance(instr, Add): res = op_add(state)
        elif isinstance(instr, Sub): res = op_sub(state)
        elif isinstance(instr, Mul): res = op_mul(state)
        elif isinstance(instr, Div): res = op_div(state)
        elif isinstance(instr, Mod): res = op_mod(state)
        elif isinstance(instr, Neg): res = op_neg(state)

        elif isinstance(instr, Eq): res = op_eq(state)
        elif isinstance(instr, Neq): res = op_neq(state)
        elif isinstance(instr, Lt): res = op_lt(state)
        elif isinstance(instr, Gt): res = op_gt(state)
        elif isinstance(instr, Le): res = op_le(state)
        elif isinstance(instr, Ge): res = op_ge(state)
        elif isinstance(instr, And): res = op_and(state)
        elif isinstance(instr, Or): res = op_or(state)
        elif isinstance(instr, Not): res = op_not(state)

        elif isinstance(instr, Cons): res = op_cons(state)
        elif isinstance(instr, Head): res = op_head(state)
        elif isinstance(instr, Tail): res = op_tail(state)
        elif isinstance(instr, ListMake): res = op_list_make(instr.val, state)
        elif isinstance(instr, Len): res = op_len(state)
        elif isinstance(instr, IsEmpty): res = op_is_empty(state)
        elif isinstance(instr, Nth): res = op_nth(state)
        elif isinstance(instr, Insert): res = op_insert(state)
        elif isinstance(instr, Remove): res = op_remove(state)
        elif isinstance(instr, Contains): res = op_contains(state)
        elif isinstance(instr, Append): res = op_append(state)
        elif isinstance(instr, Reverse): res = op_reverse(state)

        elif isinstance(instr, Load): res = op_load(instr.val, state)
        elif isinstance(instr, Store): res = op_store(instr.val, state)
        elif isinstance(instr, Define): res = op_define(instr.val, state)

        elif isinstance(instr, Jump): res = op_jump(instr.val, state)
        elif isinstance(instr, JumpIfTrue): res = op_jump_if_true(instr.val, state)
        elif isinstance(instr, JumpIfFalse): res = op_jump_if_false(instr.val, state)

        elif isinstance(instr, Call): res = op_call(instr.func_idx, instr.arg_count, state)
        elif isinstance(instr, Return): res = op_return(state)
        elif isinstance(instr, Closure): res = op_closure(instr.val, state)
        elif isinstance(instr, LoadArg): res = op_load_arg(instr.val, state)

        elif isinstance(instr, Print):
            if not state.stack:
                res = "Error: Print requires a value on the stack"
            else:
                val = state.stack[-1]
                print(val)
                res = state

        elif isinstance(instr, Input):
            try:
                line = input()
                res = op_input(line, state)
            except EOFError:
                res = op_input("", state)

        elif isinstance(instr, MakeTuple): res = op_make_tuple(instr.val, state)
        elif isinstance(instr, TupleGet): res = op_tuple_get(instr.val, state)
        elif isinstance(instr, MakeArray): res = op_make_array(instr.val, state)
        elif isinstance(instr, ArrayGet): res = op_array_get(state)
        elif isinstance(instr, ArraySet): res = op_array_set(state)
        elif isinstance(instr, MakeMap): res = op_make_map(instr.val, state)
        elif isinstance(instr, MapGet): res = op_map_get(state)
        elif isinstance(instr, MapSet): res = op_map_set(state)
        elif isinstance(instr, MakeStruct): res = op_make_struct(instr.val, state)
        elif isinstance(instr, StructGet): res = op_struct_get(instr.name, state)
        elif isinstance(instr, StructSet): res = op_struct_set(instr.name, state)

        elif isinstance(instr, OpenFile): res = op_open_file(state)
        elif isinstance(instr, ReadFile): res = op_read_file(state)
        elif isinstance(instr, WriteFile): res = op_write_file(state)
        elif isinstance(instr, CloseFile): res = op_close_file(state)

        else:
            res = f"Error: Unknown instruction {instr}"

        if isinstance(res, str):
            print(f"Runtime Error: {res}")
            state.is_running = False
            break
            
        state.ip += 1
