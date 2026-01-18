from typing import Union
import os
from execution.state import VMState
from vm_types import StringVal, FileVal

def op_open_file(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: OpenFile requires [String path, String mode] on the stack"
    
    mode_val = state.stack.pop()
    path_val = state.stack.pop()
    
    if not isinstance(mode_val, StringVal) or not isinstance(path_val, StringVal):
        state.stack.append(path_val)
        state.stack.append(mode_val)
        return "Error: OpenFile requires [String path, String mode] on the stack"
        
    mode = mode_val.val
    path = path_val.val
    
    if mode == "r":
        if os.path.exists(path) and os.path.isfile(path):
             state.stack.append(FileVal(path))
        else:
             return f"Error: File not found: {path}"
    elif mode == "w":
        state.stack.append(FileVal(path))
    elif mode == "a":
        state.stack.append(FileVal(path))
    else:
        return f"Error: Invalid file mode '{mode}'"
        
    return state

def op_read_file(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: ReadFile requires a FileVal on the stack"
        
    file_val = state.stack.pop()
    
    if not isinstance(file_val, FileVal):
         state.stack.append(file_val)
         return "Error: ReadFile requires a FileVal on the stack"
         
    path = file_val.val
    try:
        with open(path, 'r') as f:
            content = f.read()
        state.stack.append(StringVal(content))
    except Exception as e:
        return f"Error: Read failed for '{path}': {str(e)}"
        
    return state

def op_write_file(state: VMState) -> Union[VMState, str]:
    if len(state.stack) < 2:
        return "Error: WriteFile requires [String content, FileVal path] on the stack"
    
    content_val = state.stack.pop()
    file_val = state.stack.pop()
    
    if not isinstance(content_val, StringVal) or not isinstance(file_val, FileVal):
        state.stack.append(file_val)
        state.stack.append(content_val)
        return "Error: WriteFile requires [String content, FileVal path] on the stack"
        
    try:
        with open(file_val.val, 'w') as f:
            f.write(content_val.val)
    except Exception as e:
        return f"Error: Write failed for '{file_val.val}': {str(e)}"
        
    return state

def op_close_file(state: VMState) -> Union[VMState, str]:
    if not state.stack:
        return "Error: CloseFile requires a FileVal on the stack"
        
    val = state.stack.pop()
    
    if not isinstance(val, FileVal):
        state.stack.append(val)
        return "Error: CloseFile requires a FileVal on the stack"
        
    return state
