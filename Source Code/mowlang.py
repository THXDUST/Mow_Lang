import sys
import time

program_filepath = ""
if len(sys.argv) > 1: program_filepath = sys.argv[1]
else: program_filepath = False
is_debug = len(sys.argv) > 2 and (sys.argv[2].lower() == "debug" or sys.argv[2].lower() == "true")

__CONSOLE_MODE_HEADER_TEXT = "MowLang interpreter V0.7.4 (02/27/2025 - 21:42:25 PM UTC) - (C) by Wesley Louzada (THXDUST) (https://github.com/THXDUST)\nFor further information please check the documentation or use HELP command."

def __PRINT_ERROR(Line, ErrorPart, Type = "INVALID KEYWORD ERROR", Console=False):
    if Console:
        print(f"ERROR: '{ErrorPart}', {Type}")
    else:
        print(f"{Type}; LINE: {Line}; FILE: {program_filepath}; KEYWORD: {ErrorPart}")
        print(f"\n\nProgram left in {time.time() - start_time} seconds due to an error.")
        exit()

def __CHECK_KEYWORD(CLine="", Key="", CheckType=1, Console=False):
    VALID_KEYS = {
                "ADD", "SUB", "MUL", "JUMP", "JUMP.EQ.0", "JUMP.GT.0",
                "JUMP.LT.0", "READ", "", "PRINT", "EXIT", "PUSH", "-->",
                "POP", "NEG", "INC", "DEC", "SQRT", "ABS", "MAX", "MIN",
                "CMP", "MOD", "DIV", "EXP", "STORE", "LOAD", "PRINT.VARS",
                "RANDOM", "HELP", "SWAP", "DUP", "PRINT.STACK", "ROT",
                "CLEAR", "ALLOC", "TIME", "TIME.SINCE", "WAIT", "ALLOC.DESTROY"
                }
    if not (Key in VALID_KEYS) and not (Key.endswith(":")) and CheckType == 1 and not (Key[(len(Key)-1):] == ":"):
        __PRINT_ERROR(CLine, Key, f"INVALID KEYWORD ERROR", Console)
    elif CheckType == 2:
        if (Key in {"JUMP", "JUMP.EQ.0", "JUMP.GT.0", "JUMP.LT.0", "PRINT", "PUSH", "LOAD", "STORE", "RANDOM", "ALLOC", "WAIT"}) and not (Key[(len(Key)-1):] == ":"):
            return True
        else:
            return False

def commands():
    command = {
        "ADD": "Adds two numbers",
        "SUB": "Subtracts the second number from the first",
        "MUL": "Multiplies two numbers",
        "JUMP": "Jumps to the line with the given label",
        "JUMP.EQ.0": "Jumps to the line with the given label if the top number on the stack is equal to zero",
        "JUMP.GT.0": "Jumps to the line with the given label if the top number on the stack is greater than zero",
        "JUMP.LT.0": "Jumps to the line with the given label if the top number on the stack is less than zero",
        "READ": "Reads a number from standard input and pushes it onto the stack",
        "PRINT": "Prints the top number on the stack",
        "EXIT": "Exits the program",
        "PUSH": "Pushes a number onto the stack",
        "-->": "Adds a comment",
        "POP": "Pops the top number from the stack",
        "NEG": "Negates the top number on the stack",
        "INC": "Increments the top number on the stack by one",
        "DEC": "Decrements the top number on the stack by one",
        "SQRT": "Pushes the square root of the top number on the stack",
        "ABS": "Pushes the absolute value of the top number on the stack",
        "MAX": "Pushes the larger of the two top numbers on the stack",
        "MIN": "Pushes the smaller of the two top numbers on the stack",
        "CMP": "Pushes 1 if the top two numbers on the stack are equal, 0 otherwise",
        "MOD": "Pushes the remainder of the division of the top two numbers on the stack",
        "DIV": "Pushes the integer division of the top two numbers on the stack",
        "EXP": "Pushes the exponentiation of the top two numbers on the stack",
        "STORE": "Stores the top number on the stack in a variable",
        "LOAD": "Loads the value of a variable onto the stack",
        "PRINT.VARS": "Prints all the variables and their values",
        "RANDOM": "Pushes a random number between 0 and 1 onto the stack",
        "HELP": "Displays help information for the available commands",
        "SWAP": "Swaps the top two numbers on the stack",
        "PRINT.STACK": "Prints the entire stack",
        "ROT": "Rotates the top three numbers on the stack",
        "CLEAR": "Clears the stack",
        "ALLOC": "Allocates a memory block of the specified size and pushes its address onto the stack",
        "TIME": "Pushes the current time in seconds since the program started onto the stack",
        "TIME.SINCE": "Pushes the time in seconds since the given address was pushed onto the stack onto the stack",
        "WAIT": "Waits for a specified number of seconds",
        "ALLOC.DESTROY": "Destroys the memory block at the given address and pops its address from the stack"
    }

    for command, description in command.items():
        print(f"{command}: {description}")

    print("\n")
    return 1

class Stack:
    def __init__(self, size):
        self.buf = [0 for _ in range(size)]
        self.sp = -1

    def push(self, number):
        try:
            self.sp += 1
            self.buf[self.sp] = number
        except IndexError as e:
            print(f"Error: Stack overflow. Try allocating more memory.")
            exit()

    def pop(self):
        number = self.buf[self.sp]
        self.sp -= 1
        return number
    
    def top(self):
        return self.buf[self.sp]
    
    def dup(self):
        if self.sp >= 0:
            self.push(self.top())

    def swap(self):
        if self.sp >= 1:
            self.buf[self.sp], self.buf[self.sp - 1] = self.buf[self.sp - 1], self.buf[self.sp]
    
    def clear(self):
        self.sp = -1

    def print_stack(self):
        print(f"Stack: {self.buf[:self.sp + 1]}")

    def rot(self):
        if self.sp >= 2:
            self.buf[self.sp], self.buf[self.sp - 1], self.buf[self.sp - 2] = self.buf[self.sp - 2], self.buf[self.sp - 1], self.buf[self.sp]
        elif self.sp >= 1:
            self.buf[self.sp], self.buf[self.sp - 1] = self.buf[self.sp - 1], self.buf[self.sp]

    def destroy(self):
        del self.buf

def interpret(code, stack):
    __LINES = []
    __PURE_LINES = []
    __PURE_LINES.append(code)

    for line in __PURE_LINES:
        __LINES.append(line.split("\n")[0])

    CLine = 0

    for line in __LINES:
        CLine += 1

        Key = line.split(" ")[0]

        __CHECK_KEYWORD(CLine, Key, 1, True)

    program = []
    token_counter = 0

    for line in __LINES:
        if line.startswith("-->"):
            continue
    
        parts = line.split()
        if not parts:
            continue

        opcode = parts[0]

        if opcode.endswith(":"):
            print("The console mode does not support jump related operations yet, sorry!")
            continue

        program.append(opcode)
        token_counter += 1

        if opcode == "PUSH":
            if len(parts) > 1:
                number = int(parts[1])
                program.append(number)
                token_counter += 1
            else:
                raise ValueError(f"Expected argument for PUSH at line: {line}")
        elif opcode in {"JUMP.EQ.0", "JUMP.GT.0", "JUMP.LT.0", "JUMP", "STORE", "LOAD", "PRINT", "RANDOM"}:
            if len(parts) > 1:
                arg = ' '.join(parts[1:])  # Join all parts after the opcode as the argument
                program.append(arg)
                token_counter += 1
            elif opcode == "PRINT" or opcode == "RANDOM":
                program.append("")  # Add an empty argument for PRINT if no argument is provided

    pc = 0


    if __CHECK_KEYWORD(None, program[pc], 2) and program[pc] != "RANDOM":
        _CURRENT_LINE = __LINES.index(str(program[pc]) + " " + str(program[pc + 1]))
    elif program[pc] != "RANDOM":
        _CURRENT_LINE = __LINES.index(str(program[pc]))
    opcode = program[pc]
    pc += 1

    if opcode == "PUSH":
        number = program[pc]
        stack.push(number)
    elif opcode == "HELP":
        commands()
    elif opcode == "POP":
        stack.pop()
    elif opcode == "ADD":
        a = stack.pop()
        b = stack.pop()
        stack.push(a + b)
    elif opcode == "SUB":
        a = stack.pop()
        b = stack.pop()
        stack.push(b - a)
    elif opcode == "MUL":
        a = stack.pop()
        b = stack.pop()
        stack.push(a * b)
    elif opcode == "DIV":
        a = stack.pop()
        b = stack.pop()
        stack.push(b // a)
    elif opcode == "MOD":
        a = stack.pop()
        b = stack.pop()
        stack.push(b % a)
    elif opcode == "EXP":
        a = stack.pop()
        b = stack.pop()
        stack.push(b ** a)
    elif opcode == "NEG":
        a = stack.pop()
        stack.push(-a)
    elif opcode == "INC":
        a = stack.pop()
        stack.push(a + 1)
    elif opcode == "DEC":
        a = stack.pop()
        stack.push(a - 1)
    elif opcode == "CMP":
        a = stack.pop()
        b = stack.pop()
        stack.push(int(a == b))
    elif opcode == "ABS":
        a = stack.pop()
        stack.push(abs(a))
    elif opcode == "SQRT":
        import math
        a = stack.pop()
        stack.push(math.isqrt(a))
    elif opcode == "MAX":
        a = stack.pop()
        b = stack.pop()
        stack.push(max(a, b))
    elif opcode == "MIN":
        a = stack.pop()
        b = stack.pop()
        stack.push(min(a, b))
    elif opcode == "PRINT":
        if pc < len(program) and program[pc] != "":
            string_literal = program[pc]
            if string_literal.isdigit() or (string_literal[1:].isdigit() and string_literal[0] in '-+'):
                print(stack.top())
            else:
                try:
                    if string_literal.split("\"")[0] == "\"" and string_literal.split("\"")[2] == "\"":
                        raise Exception("MISSED DOUBLE QUOTES")
                    else:
                        print(string_literal.split("\"")[1])
                except Exception as e:
                    print(9)
                    __PRINT_ERROR(_CURRENT_LINE, "PRINT", None, True)
                    print(10)
        else:
            print(stack.top())
    elif opcode == "READ":
        number = float(input())
        stack.push(number)
    elif opcode == "JUMP.EQ.0":
        print("The console mode does not support jump operations!")
    elif opcode == "JUMP.GT.0":
        print("The console mode does not support jump operations!")
    elif opcode == "JUMP.LT.0":
        print("The console mode does not support jump operations!")
    elif opcode == "JUMP":
        print("The console mode does not support jump operations!")
    elif opcode == "DUP":
        stack.dup()
    elif opcode == "SWAP":
        stack.swap()
    elif opcode == "STORE":
        print("The console mode does not support variable operations yet, sorry!")
    elif opcode == "LOAD":
        print("The console mode does not support variable operations yet, sorry!")
    elif opcode == "PRINT.VARS":
        print("The console mode does not support variable operations yet, sorry!")
    elif opcode == "RANDOM":
        import random

        ARGS = program[pc].split(" ")

        if len(ARGS) == 0 or ARGS[0] == "":
            stack.push(random.randint(0, 100))
        elif len(ARGS) == 1:
            n = random.randint(0, int(ARGS[0]))
            stack.push(n)
        elif len(ARGS) == 2:
            arg1 = int(ARGS[0])
            arg2 = int(ARGS[1])
            stack.push(random.randint(arg1, arg2))
        else:
            __PRINT_ERROR(_CURRENT_LINE, "RANDOM", None, True)
            exit()
    elif opcode == "CLEAR":
        stack.clear()
    elif opcode == "PRINT.STACK":
        try:
            stack.print_stack()
        except AttributeError as e:
            __PRINT_ERROR(CLine, program[pc - 1], "CANNOT PRINT STACK. Make sure you allocated any memory or it hasn't been destroyed")
    elif opcode == "ROT":
        stack.rot()
    elif opcode == "ALLOC":
        ARGS = program[pc].split(" ")

        if stack != 1:
            print(f"CANNOT ALLOCATE MEMORY: Memory is already allocated for this program.")

        if len(ARGS) == 0 or ARGS[0] == "" or ARGS[0] == " ":
            stack = Stack(256)
        if len(ARGS) == 1 and not ARGS[0] == "":
            n = int(ARGS[0])
            stack = Stack(n)
                
        pc += 1
    elif opcode == "TIME":
        from datetime import datetime, timezone
        ARGS = program[pc].split(" ")
        __RAN_TIME = 1

        for arg in ARGS:
            for argument in ARGS:
                if argument == "UTC":
                    __RAN_TIME = datetime.now(timezone.utc)
                elif argument == "LOCAL":
                    __RAN_TIME = datetime.now()
            
            if __RAN_TIME == 1:
                __PRINT_ERROR(CLine, program[pc - 1], f"The TIME keyword expects either 'UTC' or 'LOCAL' as an argument")

            if arg == "FULL":
                print(__RAN_TIME)
                stack.push(__RAN_TIME.year)
                stack.push(__RAN_TIME.month)
                stack.push(__RAN_TIME.day)
                stack.push(__RAN_TIME.hour)
                stack.push(__RAN_TIME.minute)
                stack.push(__RAN_TIME.second)
                stack.push(__RAN_TIME.microsecond)
                break
            elif arg == "YEAR":
                stack.push(__RAN_TIME.year)
            elif arg == "MONTH":
                stack.push(__RAN_TIME.month)
            elif arg == "DAY":
                stack.push(__RAN_TIME.day)
            elif arg == "HOUR":
                stack.push(__RAN_TIME.hour)
            elif arg == "MINUTE":
                stack.push(__RAN_TIME.minute)
            elif arg == "SECOND":
                stack.push(__RAN_TIME.second)
            elif arg == "MICROSECOND":
                stack.push(__RAN_TIME.microsecond)
                
        pc += 1
    elif opcode == "TIME.SINCE":
        print("The console mode does not support this method!")
    elif opcode == "WAIT":
        ARGS = program[pc].split(" ")

        if ARGS[0] != "" and ARGS[0] != " ":
            try:
                wait_time = float(ARGS[0])
                time.sleep(wait_time)
            except ValueError:
                __PRINT_ERROR(CLine, program[pc - 1], "Invalid argument for WAIT keyword. Expected a number")
        else:
            wait_time = float(stack.top())
            if wait_time < 0.001:
                __PRINT_ERROR(CLine, program[pc - 1], f"Number way to small for WAIT command: {wait_time} seconds")
            time.sleep((wait_time))

        pc += 1
    elif opcode == "ALLOC.DESTROY":
        try:
            stack.destroy()
            stack = 1
        except:
            __PRINT_ERROR(CLine, program[pc - 1], "CANNOT DELETE STACK. Make sure you have any allocated memory of it hasn't been detroyed")

def Thread():
        stack = 1
        __LINES = []
        __PURE_LINES = []

        with open(program_filepath, "r") as program_file:
            for line in program_file.readlines():
                __PURE_LINES.append(line)
            #print(__PURE_LINES)
            for line in __PURE_LINES:
                #print(line.split("\n")[0])
                __LINES.append(line.split("\n")[0])

        CLine = 0

        if __LINES[0].split(" ")[0] != "ALLOC":
            __PRINT_ERROR(1, __LINES[0].split(" ")[0], "MEMORY MUST BE ALLOCATED FOR THE PROGRAM TO RUN")

        for line in __LINES:
            CLine += 1

            Key = line.split(" ")[0]

            __CHECK_KEYWORD(CLine, Key)

        program = []
        token_counter = 0
        label_tracker = {}

        for line in __LINES:
            if line.startswith("-->"):
                continue
    
            parts = line.split()
            if not parts:
                continue

            opcode = parts[0]

            if opcode.endswith(":"):
                label_tracker[opcode[:-1]] = token_counter
                continue

            program.append(opcode)
            token_counter += 1

            if opcode == "PUSH":
                if len(parts) > 1:
                    number = int(parts[1])
                    program.append(number)
                    token_counter += 1
                else:
                    raise ValueError(f"Expected argument for PUSH at line: {line}")
            elif opcode in {"JUMP.EQ.0", "JUMP.GT.0", "JUMP.LT.0", "JUMP", "STORE", "LOAD", "PRINT", "RANDOM", "ALLOC", "TIME", "WAIT"}:
                if len(parts) > 1:
                    arg = ' '.join(parts[1:])  # Join all parts after the opcode as the argument
                    program.append(arg)
                    token_counter += 1
                elif opcode == "PRINT" or opcode == "RANDOM" or opcode == "ALLOC" or opcode == "TIME" or opcode == "WAIT":
                    program.append("")  # Add an empty argument for PRINT if no argument is provided

        variables = {}

        def store_variable(name):
            value = stack.pop()
            variables[name] = value

        def load_variable(name):
            value = variables[name]
            stack.push(value)

        def print_variables():
            print("Variables:")
            for name, value in variables.items():
                print(f"{name} = {value}")

        pc = 0

        # print(__PURE_LINES[0].split("\n"))

        while program[pc] != "EXIT":
            __SKIP_VERIFICATION = {"RANDOM", "ALLOC", "TIME", "WAIT"}

            if __CHECK_KEYWORD(None, program[pc], 2) and not program[pc] in __SKIP_VERIFICATION:
                _CURRENT_LINE = __LINES.index(str(program[pc]) + " " + str(program[pc + 1]))
            elif not program[pc] in __SKIP_VERIFICATION and not program[pc].endswith(":"):
                #print(program[pc] + ".")
                #print(__LINES)
                _CURRENT_LINE = __LINES.index(str(program[pc]))
            opcode = program[pc]
            pc += 1

            if opcode == "PUSH":
                number = program[pc]
                pc += 1
                stack.push(number)
            elif opcode == "HELP":
                __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], "Unfortuantly, the FILE READ MODE does not support the HELP command")
                pc += 1
            elif opcode == "POP":
                stack.pop()
            elif opcode == "ADD":
                a = stack.pop()
                b = stack.pop()
                stack.push(a + b)
            elif opcode == "SUB":
                a = stack.pop()
                b = stack.pop()
                stack.push(b - a)
            elif opcode == "MUL":
                a = stack.pop()
                b = stack.pop()
                stack.push(a * b)
            elif opcode == "DIV":
                a = stack.pop()
                b = stack.pop()
                stack.push(b // a)
            elif opcode == "MOD":
                a = stack.pop()
                b = stack.pop()
                stack.push(b % a)
            elif opcode == "EXP":
                a = stack.pop()
                b = stack.pop()
                stack.push(b ** a)
            elif opcode == "NEG":
                a = stack.pop()
                stack.push(-a)
            elif opcode == "INC":
                a = stack.pop()
                stack.push(a + 1)
            elif opcode == "DEC":
                a = stack.pop()
                stack.push(a - 1)
            elif opcode == "CMP":
                a = stack.pop()
                b = stack.pop()
                stack.push(int(a == b))
            elif opcode == "ABS":
                a = stack.pop()
                stack.push(abs(a))
            elif opcode == "SQRT":
                import math
                a = stack.pop()
                stack.push(math.isqrt(a))
            elif opcode == "MAX":
                a = stack.pop()
                b = stack.pop()
                stack.push(max(a, b))
            elif opcode == "MIN":
                a = stack.pop()
                b = stack.pop()
                stack.push(min(a, b))
            elif opcode == "PRINT":
                if pc < len(program) and program[pc] != "":
                    string_literal = program[pc]
                    pc += 1
                    if string_literal.isdigit() or (string_literal[1:].isdigit() and string_literal[0] in '-+'):
                        print(stack.top())
                    else:
                        try:
                            if string_literal.split("\"")[0] == "\"" and string_literal.split("\"")[2] == "\"":
                                raise Exception("MISSED DOUBLE QUOTES")
                            print(string_literal.split("\"")[1])
                        except Exception as e:
                            __PRINT_ERROR(_CURRENT_LINE, __PURE_LINES[pc])
                else:
                    print(stack.top())
            elif opcode == "READ":
                number = float(input())
                stack.push(number)
            elif opcode == "JUMP.EQ.0":
                number = stack.top()
                if number == 0:
                    pc = label_tracker[program[pc]]
                else:
                    pc += 1
            elif opcode == "JUMP.GT.0":
                number = stack.top()
                if number > 0:
                    pc = label_tracker[program[pc]]
                else:
                    pc += 1
            elif opcode == "JUMP.LT.0":
                number = stack.top()
                if number < 0:
                    pc = label_tracker[program[pc]]
                else:
                    pc += 1
            elif opcode == "JUMP":
                pc = label_tracker[program[pc]]
            elif opcode == "DUP":
                stack.dup()
            elif opcode == "SWAP":
                stack.swap()
            elif opcode == "STORE":
                variable_name = program[pc]
                store_variable(variable_name)
                pc += 1
            elif opcode == "LOAD":
                variable_name = program[pc]
                if variable_name in variables:
                    load_variable(variable_name)
                else:
                    raise KeyError(f"Variable '{variable_name}' not found.")
                pc += 1
            elif opcode == "PRINT.VARS":
                print_variables()
            elif opcode == "RANDOM":
                import random

                ARGS = program[pc].split(" ")

                if len(ARGS) == 0 or ARGS[0] == "":
                    stack.push(random.randint(0, 100))
                elif len(ARGS) == 1:
                    n = random.randint(0, int(ARGS[0]))
                    stack.push(n)
                elif len(ARGS) == 2:
                    arg1 = int(ARGS[0])
                    arg2 = int(ARGS[1])
                    stack.push(random.randint(arg1, arg2))
                else:
                    __PRINT_ERROR(CLine, program[pc - 1], "Invalid arguments for RANDOM keyword")
                    exit()
        
                pc += 1
            elif opcode == "CLEAR":
                stack.clear()
            elif opcode == "PRINT.STACK":
                try:
                    stack.print_stack()
                except AttributeError as e:
                    __PRINT_ERROR(CLine, program[pc - 1], "CANNOT PRINT STACK. Make sure you allocated any memory or it hasn't been destroyed")
            elif opcode == "ROT":
                stack.rot()
            elif opcode == "ALLOC":
                ARGS = program[pc].split(" ")

                if stack != 1:
                    print(f"CANNOT ALLOCATE MEMORY: Memory is already allocated for this program.")

                if len(ARGS) == 0 or ARGS[0] == "" or ARGS[0] == " ":
                    stack = Stack(256)
                if len(ARGS) == 1 and not ARGS[0] == "":
                    n = int(ARGS[0])
                    stack = Stack(n)
                
                pc += 1
            elif opcode == "TIME":
                from datetime import datetime, timezone
                ARGS = program[pc].split(" ")
                __RAN_TIME = 1

                for arg in ARGS:
                    for argument in ARGS:
                        if argument == "UTC":
                            __RAN_TIME = datetime.now(timezone.utc)
                        elif argument == "LOCAL":
                            __RAN_TIME = datetime.now()
                    
                    if __RAN_TIME == 1:
                        __PRINT_ERROR(CLine, program[pc - 1], f"The TIME keyword expects either 'UTC' or 'LOCAL' as an argument")

                    if arg == "FULL":
                        print(__RAN_TIME)
                        stack.push(__RAN_TIME.year)
                        stack.push(__RAN_TIME.month)
                        stack.push(__RAN_TIME.day)
                        stack.push(__RAN_TIME.hour)
                        stack.push(__RAN_TIME.minute)
                        stack.push(__RAN_TIME.second)
                        stack.push(__RAN_TIME.microsecond)
                        break
                    elif arg == "YEAR":
                        stack.push(__RAN_TIME.year)
                    elif arg == "MONTH":
                        stack.push(__RAN_TIME.month)
                    elif arg == "DAY":
                        stack.push(__RAN_TIME.day)
                    elif arg == "HOUR":
                        stack.push(__RAN_TIME.hour)
                    elif arg == "MINUTE":
                        stack.push(__RAN_TIME.minute)
                    elif arg == "SECOND":
                        stack.push(__RAN_TIME.second)
                    elif arg == "MICROSECOND":
                        stack.push(__RAN_TIME.microsecond)
                
                pc += 1
            elif opcode == "TIME.SINCE":
                print(f"Time elapsed since the program started: {time.time() - start_time} seconds.")
                stack.push(time.time() - start_time)
            elif opcode == "WAIT":
                ARGS = program[pc].split(" ")

                if ARGS[0] != "" and ARGS[0] != " ":
                    try:
                        wait_time = float(ARGS[0])
                        time.sleep(wait_time)
                    except ValueError:
                        __PRINT_ERROR(CLine, program[pc - 1], "Invalid argument for WAIT keyword. Expected a number")
                else:
                    wait_time = float(stack.top())
                    if wait_time < 0.001:
                        __PRINT_ERROR(CLine, program[pc - 1], f"Number way to small for WAIT command: {wait_time} seconds")
                    time.sleep((wait_time))

                pc += 1
            elif opcode == "ALLOC.DESTROY":
                try:
                    stack.destroy()
                    stack = 1
                except:
                    __PRINT_ERROR(CLine, program[pc - 1], "CANNOT DELETE STACK. Make sure you have any allocated memory of it hasn't been detroyed")

        print(f"\n\n-\nProgram concluded in: {time.time() - start_time} seconds.")
        exit()

def Console():
    print(__CONSOLE_MODE_HEADER_TEXT)
    stack = Stack(256)
    while True:
        try:
            code = input("> ")
            if code.strip() == "EXIT":
                break
            interpret(code, stack)
        except Exception as e:
            print(f"Error: {str(e)}")

def Main():
    global start_time
    start_time = time.time()
    if is_debug and program_filepath is not False:
        print("DEBUG MODE IS ACTIVE.\n-----\n\n")
        time.sleep(1)
        try:
            Thread()
        except Exception as e:
            __PRINT_ERROR(-1, "Inside main thread", str(e), True)
    elif program_filepath is not False:
        Thread()
    else:
        Console()

if __name__ == "__main__":
    Main()
