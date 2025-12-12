import sys

program_filepath = sys.argv[1]

program_lines = []
with open(program_filepath, "r") as program_file:
    program_lines = [line.split('-->')[0].strip() for line in program_file.readlines()]

program = []
token_counter = 0
label_tracker = {}
for line in program_lines:
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
    elif opcode in {"JUMP.EQ.0", "JUMP.GT.0", "JUMP.LT.0", "JUMP", "STORE", "LOAD", "PRINT"}:
        if len(parts) > 1:
            arg = ' '.join(parts[1:])
            program.append(arg)
            token_counter += 1
        elif opcode == "PRINT":
            program.append("")

class Stack:

    def __init__(self, size):
        self.buf = [0 for _ in range(size)]
        self.sp = -1

    def push(self, number):
        self.sp += 1
        self.buf[self.sp] = number

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
stack = Stack(256)

while program[pc] != "EXIT":
    opcode = program[pc]
    pc += 1

    if opcode == "PUSH":
        number = program[pc]
        pc += 1
        stack.push(number)
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
                print(string_literal)
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

print(" ")
print(" ")
print(" ")
input("Press Enter to exit...")