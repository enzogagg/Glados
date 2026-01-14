from typing import Union
from execution.state import VMState
from vm_types import StringVal

def op_input(line: str, state: VMState) -> Union[VMState, str]:
    state.stack.append(StringVal(line))
    return state
