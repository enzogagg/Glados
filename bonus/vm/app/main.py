import sys
import os

# Adjust path to import from src
current_dir = os.path.dirname(os.path.abspath(__file__))
src_path = os.path.join(current_dir, '..', 'src')
sys.path.insert(0, src_path)

try:
    from parser import Parser, BytecodeFile
    from disassembler import disassemble
except ImportError as e:
    print(f"Error: Could not import modules: {e}")
    sys.exit(1)

# Placeholder imports for future modules
from execution.state import new_vm_state
from execution.loop import exec_loop

def print_help():
    print("USAGE: ./glados-vm [options] [file.cbc]")
    print("       file.cbc   file to execute")
    print("       -d         print disassembly before execution")

def main():
    args = sys.argv[1:]
    
    flags = [arg for arg in args if arg == '-d']
    regular_args = [arg for arg in args if arg != '-d']
    debug = bool(flags)

    input_data = b""
    vm_args = []

    if regular_args and regular_args[0] == '-h':
        print_help()
        sys.exit(0)

    if not regular_args:
        # Read from stdin
        try:
            if not sys.stdin.isatty():
                input_data = sys.stdin.buffer.read()
            vm_args = []
        except Exception as e:
            print(f"Error reading from stdin: {e}")
            sys.exit(84)
    else:
        file_path = regular_args[0]
        try:
            with open(file_path, 'rb') as f:
                input_data = f.read()
            vm_args = regular_args
        except FileNotFoundError:
            print(f"Error: File '{file_path}' not found")
            sys.exit(84)
        except Exception as e:
            print(f"Error reading file '{file_path}': {e}")
            sys.exit(84)

    if not input_data:
        print("Error: No input provided")
        print_help()
        sys.exit(84)

    try:
        parser = Parser(input_data)
        bytecode = parser.parse_bytecode()
    except Exception as e:
        print(f"Error parsing bytecode: {e}")
        sys.exit(84)

    if debug:
        disassemble(bytecode)

    state = new_vm_state(bytecode.instructions, bytecode.pool, bytecode.functions, vm_args)
    exec_loop(state)
    sys.exit(0)

if __name__ == "__main__":
    main()
