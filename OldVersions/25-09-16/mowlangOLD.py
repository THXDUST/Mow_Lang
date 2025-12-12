import sys, os
import time

program_filepath = ""
if len(sys.argv) > 1: program_filepath = sys.argv[1]
else: program_filepath = False
is_debug = len(sys.argv) > 2 and (sys.argv[2].lower() == "debug" or sys.argv[2].lower() == "true")
debugread = False

__CONSOLE_MODE_HEADER_TEXT = "MowLang interpreter V0.7.4 (02/27/2025 - 21:42:25 PM UTC) - (C) by Wesley Louzada (THXDUST) (https://github.com/THXDUST)\nFor further information please check the documentation or use HELP command."

def __PRINT_ERROR(Line, ErrorPart, Type = "INVALID KEYWORD ERROR", Console=False):
    if Console or ConsoleMode:
        print(f"ERROR: '{ErrorPart}', {Type}")
    else:
        print(f"{Type}; LINE: {Line}; FILE: {program_filepath}; KEYWORD: {ErrorPart}")
        if is_debug:
            print(f"\n\nProgram left in {time.time() - start_time} seconds due to an error.")
        exit()

def __CHECK_FOR_METHODS(FullKey):
    print(FullKey)
    global ConsoleMode
    if ConsoleMode:
        try:
            separed_key = FullKey.split(".")
            try:
                args = FullKey.split(" ")
            except:
                separed_key = [FullKey]
        except Exception:
            separed_key = [FullKey]

        if len(args) > 1:
            return args[0], args[1]
        elif len(separed_key) > 1:
            return separed_key[0], separed_key[1]
        else:
            return [FullKey, False, []]

    try:
        separed_key = FullKey.split(".")
    except Exception:
        separed_key = [FullKey]
    
    print(separed_key)

    if len(separed_key) > 1:
        return separed_key[0], separed_key[1]
    else:
        return [FullKey, False]

def store_variable(name):
    global stack, variables
    value = stack.pop()
    variables[name] = value
    print(name, variables)

def load_variable(name):
    global stack, variables
    value = variables[name]
    stack.push(value)
    print(name, variables)

def print_variables():
    global variables
    print("Variables:")
    for name, value in variables.items():
        print(f"{name} = {value}")
    print(variables)

def __CHECK_KEYWORD(CLine="", Key="", CheckType=1, Console=False):
    VALID_KEYS = {
                "ADD", "SUB", "MUL", "JUMP", "JUMP_EQ_0", "JUMP_GT_0",
                "JUMP_LT_0", "READ", "", "PRINT", "EXIT", "PUSH", "-->",
                "POP", "NEG", "INC", "DEC", "SQRT", "ABS", "MAX", "MIN",
                "CMP", "MOD", "DIV", "EXP", "STORE", "LOAD", "RANDOM",
                "HELP", "SWAP", "DUP", "ROT", "CLEAR", "ALLOC", "TIME",
                "WAIT", "IF", "ELSE", "ENDIF", "JUMP_NE_0", "DEBUG"
                }
    if not (Key in VALID_KEYS) and not (Key.endswith(":")) and CheckType == 1 and not (Key[(len(Key)-1):] == ":") and not (__CHECK_FOR_METHODS(Key)[0] in VALID_KEYS):
        __PRINT_ERROR(CLine, Key, f"INVALID KEYWORD ERROR", Console)
    elif CheckType == 2:
        if (Key in {"JUMP", "JUMP_EQ_0", "JUMP_GT_0", "JUMP_LT_0", "PRINT", "PUSH", "LOAD", "STORE", "RANDOM", "ALLOC", "WAIT"}) and not (Key[(len(Key)-1):] == ":"):
            return True
        else:
            return False

def commands():
    command = {
        "ADD": "Adds two numbers",
        "SUB": "Subtracts the second number from the first",
        "MUL": "Multiplies two numbers",
        "JUMP": "Jumps to the line with the given label",
        "JUMP_EQ_0": "Jumps to the line with the given label if the top number on the stack is equal to zero",
        "JUMP_GT_0": "Jumps to the line with the given label if the top number on the stack is greater than zero",
        "JUMP_LT_0": "Jumps to the line with the given label if the top number on the stack is less than zero",
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
        "ALLOC.DESTROY": "Destroys the memory block at the given address and pops its address from the stack",
        "IF": "Runs the following block code if the condition is true.",
        "ELSE": "Runs the following block code if the previous IF statement is false.",
        "ENDIF": "Ends the IF statement."
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

    def round(self):
        try:
            if self.sp >= 0:
                self.buf[self.sp] = round(self.buf[self.sp])
                return self.buf[self.sp]
        except Exception as e:
            return

def Run(keyword):
    global stack, ifelse_tracker, CLine, __PURE_LINES, variables, label_tracker, last_result, _CURRENT_LINE, pc, program, ConsoleMode, debugread

    if debugread:
        print(f"Executing {keyword, program[pc]} at pc={pc}")

    def PUSH(METHOD = False):
        global pc
        global stack
        number = program[pc]
        pc += 1
        stack.push(number)

    def HELP(METHOD = False):
        global pc
        global stack
        if ConsoleMode:
            commands()
        else:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], "Unfortuantly, the FILE READ MODE does not support the HELP command")
            pc += 1

    def POP(METHOD = False):
        global stack
        stack.pop()

    def ADD(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(a + b)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def SUB(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(b - a)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def MUL(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(a * b)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def DIV(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(b / a)
        except ZeroDivisionError:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], "Message: Division by zero")
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def MOD(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(b % a)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def EXP(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(b ** a)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def NEG(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        try:
            stack.push(-a)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the top value is an integer. Error: {str(e)}")

    def INC(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        try:
            stack.push(a + 1)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the top value is an integer. Error: {str(e)}")

    def DEC(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        try:
            stack.push(a - 1)
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the top value is an integer. Error: {str(e)}")

    def ABS(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        try:
            stack.push(abs(a))
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the top value is an integer. Error: {str(e)}")

    def SQRT(METHOD = False):
        global pc
        global stack
        import math
        a = stack.pop()
        try:
            stack.push(math.isqrt(a))
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the top value is a positive integer. Error: {str(e)}")

    def MAX(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(max(a, b))
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def MIN(METHOD = False):
        global pc
        global stack
        a = stack.pop()
        b = stack.pop()
        try:
            stack.push(min(a, b))
        except Exception as e:
            __PRINT_ERROR(CLine, __PURE_LINES[pc - 1], f"Message: Make sure the 2 top values are integers. Error: {str(e)}")

    def PRINT(METHOD=False):
        global pc
        global stack
        global variables
        global program
        if METHOD == "VARS":
            print_variables()
            return
        elif METHOD == "STACK":
            try:
                stack.print_stack()
            except AttributeError as e:
                __PRINT_ERROR(CLine, program[pc - 1], "CANNOT PRINT STACK. Make sure you allocated any memory or it hasn't been destroyed")
            return
        

        if pc < len(program) and program[pc] != "":
            print("a1")
            string_literal = program[pc]
            print("a2")
            pc += 1
            print("a3")
            if string_literal.isdigit() or (string_literal[1:].isdigit() and string_literal[0] in '-+'):
                print("a4")
                print(stack.top())
                print("a5")
                return

            if variables.get(string_literal) is not None:
                print("a6")
                print(variables[string_literal])
                print("a7")
                return

            if string_literal.startswith("\"") and string_literal.endswith("\""):
                print("a8")
                print(''.join(string_literal.split("\"")[1:-1]))
                print("a9")
                return
            
            print("b1")
        else:
            print(stack.top())

    def READ(METHOD = False):
        global stack
        number = input()
        try:
            stack.push(float(number))
        except ValueError:
            stack.push(number)

    def JUMP_EQ_0(METHOD=False):
        global pc, stack
        if not ConsoleMode:
            try:
                ARGS = program[pc].split(" ")
                if debugread:
                    print(ARGS, label_tracker[ARGS[0]])

                if stack.top() == 0:
                    pc = label_tracker[ARGS[0]]
            except Exception as e:
                print(e)

            pc += 1
        else:
            __PRINT_ERROR(CLine, __PURE_LINES, "Console mode does not support jump operations")

    def JUMP_GT_0(METHOD = False):
        global pc
        global stack
        if not ConsoleMode:
            try:
                ARGS = program[pc].split(" ")
                if debugread:
                    print(ARGS, label_tracker[ARGS[0]])

                if stack.top() > 0:
                    pc = label_tracker[ARGS[0]]
            except Exception as e:
                print(e)

            pc += 1
        else:
            __PRINT_ERROR(CLine, __PURE_LINES, "Unfortunatly, the CONSOLE MODE does NOT support jump operations")

    def JUMP_LT_0(METHOD = False):
        global pc
        global stack
        if not ConsoleMode:
            number = stack.top()
            if number < 0:
                pc = label_tracker[program[pc]]
            pc += 1
        else:
            __PRINT_ERROR(CLine, __PURE_LINES, "Unfortunatly, the CONSOLE MODE does NOT support jump operations")

    def JUMP_NE_0(METHOD = False):
        global pc
        global stack
        if not ConsoleMode:
            number = stack.top()
            if number != 0:
                pc = label_tracker[program[pc]]
            pc += 1
        else:
            __PRINT_ERROR(CLine, __PURE_LINES, "Unfortunatly, the CONSOLE MODE does NOT support jump operations")

    def JUMP(METHOD = False):
        global pc
        if not ConsoleMode:
            pc = label_tracker[program[pc]]
        else:
            __PRINT_ERROR(CLine, __PURE_LINES, "Unfortunatly, the CONSOLE MODE does NOT support jump operations")

    def DUP(METHOD = False):
        global stack
        stack.dup()

    def SWAP(METHOD = False):
        global pc
        stack.swap()

    def STORE(METHOD = False):
        global pc, program, variables
        variable_name = program[pc]
        store_variable(variable_name)
        pc += 1

    def LOAD(METHOD = False):
        global pc, stack, program, variables
        variable_name = program[pc]
        if variable_name in variables:
            load_variable(variable_name)
        else:
            raise KeyError(f"Variable '{variable_name}' not found.")
        pc += 1

    def RANDOM(METHOD = False):
        global pc
        global stack
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

    def CLEAR(METHOD = False):
        global stack
        stack.clear()

    def ROT(METHOD = False):
        global stack
        stack.rot()

    def ALLOC(METHOD=False):
        global pc
        global stack
        if METHOD == "DESTROY":
            try:
                stack.destroy()
                stack = 1
            except:
                __PRINT_ERROR(CLine, program[pc - 1], "CANNOT DELETE STACK. Make sure you have any allocated memory of it hasn't been detroyed")

            return

        ARGS = program[pc].split(" ")

        if stack != 1:
            print(f"CANNOT ALLOCATE MEMORY: Memory is already allocated for this program.")

        if len(ARGS) == 0 or ARGS[0] == "" or ARGS[0] == " ":
            stack = Stack(256)
        if len(ARGS) == 1 and not ARGS[0] == "":
            n = int(ARGS[0])
            stack = Stack(n)
                
        pc += 1

    def TIME(METHOD=False):
        global pc
        global stack
        if METHOD == "SINCE":
            if not ConsoleMode:
                print(f"Time elapsed since the program started: {time.time() - start_time} seconds.")
                stack.push(time.time() - start_time)
            else:
                __PRINT_ERROR(CLine, __PURE_LINES, "Unfortunatly, the CONSOLE MODE does NOT support TIME SINCE method")
            
            return
                
        from datetime import datetime, timezone
        ARGS = program[pc].split(" ")
        __RAN_TIME = 1
        not_print = False

        for arg in ARGS:
            for argument in ARGS:
                if argument == "UTC":
                    __RAN_TIME = datetime.now(timezone.utc)
                elif argument == "LOCAL":
                    __RAN_TIME = datetime.now()
                elif argument == "PR":
                    not_print = True

            if __RAN_TIME == 1:
                __PRINT_ERROR(CLine, program[pc - 1], f"The TIME keyword expects either 'UTC' or 'LOCAL' as an argument")

            if arg == "FULL":
                if not_print:
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
                if not_print:
                    print(__RAN_TIME.year)
                stack.push(__RAN_TIME.year)
            elif arg == "MONTH":
                if not_print:
                    print(__RAN_TIME.month)
                stack.push(__RAN_TIME.month)
            elif arg == "DAY":
                if not_print:
                    print(__RAN_TIME.day)
                stack.push(__RAN_TIME.day)
            elif arg == "HOUR":
                if not_print:
                    print(__RAN_TIME.hour)
                stack.push(__RAN_TIME.hour)
            elif arg == "MINUTE":
                if not_print:
                    print(__RAN_TIME.minute)
                stack.push(__RAN_TIME.minute)
            elif arg == "SECOND":
                if not_print:
                    print(__RAN_TIME.second)
                stack.push(__RAN_TIME.second)
            elif arg == "MICROSECOND":
                if not_print:
                    print(__RAN_TIME.microsecond)
                stack.push(__RAN_TIME.microsecond)
                
        pc += 1

    def WAIT(METHOD = False):
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

    def IF(METHOD = False):
        global pc
        global stack
        if ConsoleMode:
            __PRINT_ERROR(CLine, __PURE_LINES, "Unfortunatly, the CONSOLE MODE does NOT support IF/ELSE statements")
            return
        
        equal = program[pc].split(" == ")
        less = program[pc].split(" < ")
        greater = program[pc].split(" > ")
        loe = program[pc].split(" <= ")
        goe = program[pc].split(" >= ")

        if len(equal) > 1:
            last_result = equal[0] == equal[1]
        elif len(less) > 1:
            try:
                last_result = less[0] < less[1]
            except ValueError as e:
                __PRINT_ERROR(CLine, program[pc - 1], e)
        elif len(greater) > 1:
            try:
                last_result = greater[0] > greater[1]
            except ValueError as e:
                __PRINT_ERROR(CLine, program[pc - 1], e)
        elif len(loe) > 1:
            try:
                last_result = loe[0] <= loe[1]
            except ValueError as e:
                __PRINT_ERROR(CLine, program[pc - 1], e)
        elif len(goe) > 1:
            try:
                last_result = goe[0] >= goe[1]
            except ValueError as e:
                __PRINT_ERROR(CLine, program[pc - 1], e)

        pc += 1
        ifelse_tracker["IF"].pop(0)

        if last_result == False:
            if not ifelse_tracker["ELSE"][0] == '':
                pc = ifelse_tracker["ELSE"][0]
            else:
                try:
                    pc = ifelse_tracker["ENDIF"][0]
                    ifelse_tracker["ENDIF"].pop(0)
                except IndexError:
                    __PRINT_ERROR(CLine, program[pc - 1], "Unmatched IF keyword. Missing ENDIF keyword.")
        else:
            return

    def ELSE(METHOD = False):
        if ConsoleMode:
            __PRINT_ERROR(CLine, __PURE_LINES, "Unfortunatly, the CONSOLE MODE does NOT support IF/ELSE statements")
            return
        
        ifelse_tracker["ELSE"].pop(0)
        try:
            ifelse_tracker["ENDIF"].pop(0)
        except IndexError:
            __PRINT_ERROR(CLine, program[pc - 1], "Unmatched ELSE keyword. Missing ENDIF keyword.")
        if last_result == False:
            return
        else:
            pc = ifelse_tracker["ENDIF"][0]
    
    if debugread:
        print(keyword)

    try:
        returns = __CHECK_FOR_METHODS(keyword)
        keyword, method = returns[0], returns[1]
        locals()[keyword](method)
    except KeyError:
        if not ConsoleMode:
            __PRINT_ERROR(CLine, program[pc], f"Invalid keyword: {keyword}")
        return

def interpret(code):
    global stack, ifelse_tracker, CLine, __PURE_LINES, variables, label_tracker, last_result, _CURRENT_LINE, pc, program
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
            label_tracker[opcode[:-1]] = token_counter
            continue

        program.append(opcode)
        token_counter += 1

        if opcode == "PUSH":
            if len(parts) > 1:
                number = ''
                try:
                    number = int(parts[1])
                except ValueError:
                    for word in parts[1:]:
                        number += word + " "
                    number = number[:-1]
                program.append(number)
                token_counter += 1
            else:
                raise ValueError(f"Expected argument for PUSH at line: {line}")
        elif opcode in {"JUMP_EQ_0", "JUMP_GT_0", "JUMP_LT_0", "JUMP", "STORE", "LOAD", "PRINT", "RANDOM", "ALLOC", "TIME", "WAIT", "IF", "JUMP_NE_0"}:
            if len(parts) > 1:
                arg = ' '.join(parts[1:])  # Join all parts after the opcode as the argument
                program.append(arg)
                token_counter += 1
            elif opcode == "PRINT" or opcode == "RANDOM" or opcode == "ALLOC" or opcode == "TIME" or opcode == "WAIT":
                program.append("")  # Add an empty argument for PRINT if no argument is provided

    variables = {}

    pc = 0

    __SKIP_VERIFICATION = {"RANDOM", "ALLOC", "TIME", "WAIT", "PUSH", "IF", "ELSE", "ENDIF", "JUMP_EQ_0", "JUMP_GT_0", "JUMP_LT_0", "JUMP_NE_0"}

    if __CHECK_KEYWORD(None, program[pc], 2) and not program[pc] in __SKIP_VERIFICATION:
        _CURRENT_LINE = __LINES.index(str(program[pc]) + " " + str(program[pc + 1]))
    elif not program[pc] in __SKIP_VERIFICATION and not str(program[pc] + ":") in __LINES:
        _CURRENT_LINE = __LINES.index(str(program[pc]))
    else:
        _CURRENT_LINE = None
    
    opcode = code
    pc += 1

    print(f"abc: {opcode}")

    Run(opcode)

def Thread():
    global stack, ifelse_tracker, CLine, __PURE_LINES, variables, label_tracker, last_result, _CURRENT_LINE, pc, program, debugread
    stack = 1
    __LINES = []
    __PURE_LINES = []
    _CURRENT_LINE = 0

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

    if __LINES[1].split(" ")[0] == "DEBUG":
        debugread = True

    for line in __LINES:
        CLine += 1
        Key = line.split(" ")[0]

        __CHECK_KEYWORD(CLine, Key)

    program = []
    token_counter = 0
    label_tracker = {}
    ifelse_tracker = {"IF": [], "ELSE": [''], "ENDIF": []}

    for line in __LINES:
        if line.startswith("-->"):
            continue
    
        if line == '':
            continue

        token_counter += 1

        parts = line.split()
        if not parts:
            continue

        if debugread:
            print(line, parts)

        opcode = parts[0]

        if opcode.endswith(":"):
            label_tracker[opcode[:-1]] = token_counter
            continue

        if opcode in {"IF", "ELSE", "ENDIF"}:
            if opcode == "IF":
                ifelse_tracker[opcode].append(token_counter)
            else:
                ifelse_tracker[opcode].insert(0, token_counter)

        program.append(opcode)

        if opcode == "PUSH":
            token_counter += 1
            if len(parts) > 1:
                number = ''
                try:
                    number = int(parts[1])
                except ValueError:
                    for word in parts[1:]:
                        number += word + " "
                    number = number[:-1]
                program.append(number)
            else:
                raise ValueError(f"Expected argument for PUSH at line: {line}")
        elif opcode in {"JUMP_EQ_0", "JUMP_GT_0", "JUMP_LT_0", "JUMP", "STORE", "LOAD", "PRINT", "RANDOM", "ALLOC", "TIME", "WAIT", "IF", "JUMP_NE_0"}:
            token_counter += 1
            if len(parts) > 1:
                arg = ' '.join(parts[1:])
                program.append(arg)
            elif opcode == "PRINT" or opcode == "RANDOM" or opcode == "ALLOC" or opcode == "TIME" or opcode == "WAIT":
                program.append("")

    if len(ifelse_tracker["IF"]) != len(ifelse_tracker["ENDIF"]):
        __PRINT_ERROR(CLine, "IF", "IF-ELSE-ENDIF blocks are not properly nested")

    variables = {}
    lists = {}

    pc = 0
    last_result = False

    if debugread:
        print(program)
        print(label_tracker)

    while program[pc] != "EXIT":
        opcode = program[pc]
        pc += 1

        if debugread:
            print(f"Before executing: pc={pc}, opcode={opcode}")
        Run(opcode)
        if debugread:
            print(f"After executing: pc={pc}")

    if is_debug or debugread:
        print(f"\n\n-----\nProgram concluded in: {time.time() - start_time} seconds.")
    exit()

def Console():
    print(__CONSOLE_MODE_HEADER_TEXT)
    global stack
    stack = Stack(256)
    while True:
        try:
            code = input("> ")
            if code.strip() == "EXIT":
                break
            interpret(code.strip())
        except Exception as e:
            print(f"Error: {str(e)}")

def Main():
    global ConsoleMode
    ConsoleMode = False
    if is_debug or debugread:
        global start_time
        start_time = time.time()
    if (is_debug or debugread) and program_filepath is not False:
        print("DEBUG MODE IS ACTIVE.\n-----\n\n")
        time.sleep(1)
        try:
            Thread()
        except Exception as e:
            __PRINT_ERROR(-1, "Inside main thread", str(e), True)
    elif program_filepath is not False:
        Thread()
    else:
        ConsoleMode = True
        Console()

if __name__ == "__main__":
    Main()

# 886