# MowLang Interpreter

MowLang is a stack-based interpreted language written in Python, inspired by traditional postfix and low-level programming concepts. It is designed for educational, experimental, and fun purposes.

---

## Features

- Stack-based operations  
- Variable declarations and memory storage  
- Built-in time functions  
- Modular importing of `.mow` files  
- Function definitions  
- Command-line console support  
- Minimalist syntax

---

## How to Run

There are two ways to use MowLang:

1. Python file:
   - File reader mode:
      ```bash
      python3 mowlang.py my_program.mow (debug)
      ```
   - Interactive mode:
      ```bash
      python3 mowlang.py
      ```

2. Executable file:
   - File reader mode: Drop the file to the interpreter
   - Interactive mode: Exeute the file

---

## Command Reference

| **Command** | **Description** \\ **Syntax** | **Example** |
|---|---|---|
| `-->` | Line comment. Everything after `-->` is ignored. `--> (Your comment)` | `--> This is a comment` |
| `PRINT` / `PRINTL` | Prints a value or string. Accepts variables or string literals. Inserting `L` at the end of the command jumps to the next line after it prints. | `PRINT "Hello, World!"` / `PRINTL "Hello, World!"` |
| `READ` | Prompts user input and returns it. `READ (expected input type) (optional string literal/variable)` | `READ int "Insert a number: "` |
| `STORE` | Declares a variable. Requires a type, name, and value/valid syntax. `STORE (data type) (variable name)` | `STORE int MyVar 20` |
| `LOAD` | Retrieves a variable's value. `LOAD (variable name)` | `LOAD MyVar` |
| `PAUSE` | Pauses execution. `PAUSE` | `PAUSE` |
| `TIME` | Requires method, `TIME` in case its methods are not accessd, raises error. `TIME.(method) (method args)` | `TIME.SLEEP 2 --> Sleeps for 2 seconds` |
| `IMPORT` | Imports another `.mow` file. Can be string-literal or raw filename. Can use `AS` to assign an alias to the file, however, if the interpreter version from the file is less than the minimum version from your file, it raises an error. `IMPORT (module file name with/without .mow extension) (optional alias)` | `IMPORT moduleName` / `IMPORT moduleName AS mod` |
| `#DEFINE` | Internal directive for configuration. `#DEFINE (definition name) (definition args)` | `#DEFINE NO_WARNS True --> suppresses INFO and WARN logs` |
| `LABEL` | Declares a label for use with `JUMP`. `LABEL (label name)` | `LABEL example_label` |
| `JUMP` | Jumps to a previously defined `LABEL`. `JUMP (optional comparison operator) (label name)` | `JUMP example_label` / `JUMP EQ 0 1 example_label --> Doesn't go to example_label because 0 is not equal to 1` |
| `FUNCTION` | Begins a function definition. Requires a name. `FUNCTION (function name) (optional function args)` | `FUNCTION myFunc arg1 arg2` |
| `END(IF)` | Marks the end of a `FUNCTION` or `IF`. `END (function name)` / `ENDIF` | `END myFunc` / `ENDIF` |
| `EXIT` | Terminates program execution. `EXIT` | `EXIT` |
| `STACK` | Manages stack operations, needs to access methods, otherwise it raises an error. `STACK.(method) (method args)` | `STACK.PUSH int 10` |
| `EXPORT` | Used to export functions or variables from modules (used inside the module). `EXPORT (type function/variable) (function/variable name)` | `EXPORT func myFunc` / `EXPORT var MyVar` |
| `RETURN` | Return values from functions. `RETURN (type) (value)` | `RETURN int 10` |
| `IF` | Executes the code block inside if the condition is true. `IF ( (condition) )` | `IF (True)` |
| `ELSE` | Executes the code block inside if the `IF` is false. `ELSE` | `ELSE` |
| `GLOBALS` | Returns the interpreter configuration and variables. `GLOBALS` | `GLOBALS` |

---

## Comparison operators

| **Command** | **Description** / **Syntax** | **Example** |
|---|---|---|
| `EQ` | Compares if the first value is equal to the second value. `EQ (first value), (second value)` | `EQ 0, 1 --> False`|
| `GT` | Compares if the first value is greater than the second value. `GT (first value), (second value)`. | `GT 0, 1 --> False` |
| `LT` | Compares if the first value is less than the second value. `LT (first value), (second value)`. | `LT 0, 1 --> True` |
| `GTE` | Compares if the first value is greater or equal to the second value. `GTE (first value), (second value)`. | `GTE 0, 1 --> False` |
| `LTE` | Compares if the first value is less or equal to the second value. `LTE (first value), (second value)`. | `LTE 0, 1 --> True` |

---

## Methods

| **Command.Method** | **Description** \\ **Syntax** | **Example** |
|---|---|---|
| `PRINT.VARS` / `PRINTL.VARS` | Prints all variables using format `\|file\|type\| name: value`. `PRINT.VARS` / `PRINTL.VARS`. | `PRINT.VARS` / `PRINTL.VARS` |
| `STACK.ALLOC` | Allocates memory for the stack. `STACK.ALLOC (number value higher than 0)`. | `STACK.ALLOC 16` |
| `STACK.PUSH` | Pushes values into the stack. `STACK.PUSH (type) (value)`. | `STACK.PUSH int 10` |
| `STACK.POP` | Removes and return the top value from the stack. `STACK.POP`. | `STACK.POP` |
| `STACK.TOP` | Returns the top value from the stack. `STACK.TOP`. | `STACK.TOP` |
| `STACK.PRINT` | Prints the stack until the value that the stack pointer is pointing. `STACK.PRINT` | `STACK.PRINT` |
| `STACK.PRINTALL` | Prints the full stack, even empty values. `STACK.PRINTALL` | `STACK.PRINTALL` |
| `STACK.CLEAR` | Clears the stack. `STACK.CLEAR`. | `STACK.CLEAR` |
| `STACK.ROT` | Reverses the stack values. `STACK.ROT`. | `STACK.ROT --> Stack with values 5, 4, 3, 2, 1 turns into 1, 2, 3, 4, 5` |
| `STACK.SWAP` | The first and second top values swap places. `STACK.SWAP`. | `SRACK.SWAP --> Stack with values 1, 2, 3 turns int 1, 3, 2` |
| `STACK.DUP` | Duplicates the top value. `STACK.DUP` | `STACK.DUP --> Stack with values 1, 2, 3 turns into 1, 2, 3, 3` |
| `TIME.SLEEP` | Pauses the execution for a determined time. `TIME.SLEEP (time in seconds integer/float)`. | `TIME.SLEEP 2 --> Stops the execution for 2 seconds (accept float values)` |
| `TIME.START` | Starts a stopwatch. `TIME.START`. | `TIME.START` |
| `TIME.STOP` | Stops the stopwatch. `TIME.STOP`. | `TIME.STOP` |
| `TIME.STAMP` | Returns the current time in UNIX. `TIME.STAMP`. | `TIME.STAMP` |
| `TIME.NOW` | Returns the current time, you can pass a format to format the time string. `TIME.NOW (format)`. | `TIME.NOW %H:%M:%S --> Hour:Minute:Second` |
| `TIME.SINCE` | Returns the time since the program started. `TIME.SINCE`. | `TIME.SINCE` | 


---

### Limitations:
- `LABEL`, `#DEFINE`, and `JUMP` are not supported in console mode.
- You can run `.mow` files by typing their path.

---

## Debug Mode

Enable with:

```mow
#DEFINE DEBUG ON
```

This will output runtime debugging information and internal states.

---

## Error Levels

The interpreter uses these levels for reporting issues:

- `INFO`: Informational message  
- `WARN`: Warning, non-critical  
- `ERROR`: Recoverable error  
- `CRITICAL`: Fatal error, terminates execution  

---

## Example Program

```mow
#DEFINE INPUT_PROMPT "Enter your name:"

STORE str name READ
PRINT "Hello, "
PRINT name
TIME.SLEEP 2
EXIT
```

More examples can be found at https://github.com/THXDUST/Mow_Lang

---

## Note

All `.mow` files may include a `filename:version` companion file. If missing, one will be created to match the current interpreter version.
All commands CAN run commands, it is mostly used for searching functions and variables, example: `STACK.PUSH int READ "Insert a number" --> transforms string type number to string (raises error if failed to do so) and it pushes into the stack`
If your program were written in an older version, the convert_old.py can make it runnable again.

---

## Versioning

The interpreter uses [PEP 440](https://www.python.org/dev/peps/pep-0440/) for semantic versioning. You can require specific versions with:

```mow
#DEFINE REQUIRE_VERSION 1.0.0
```