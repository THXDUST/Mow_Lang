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
- Minimalist syntax with robust error handling

---

## How to Run

There are two ways to use MowLang:

1. Run `.mow` files through the interpreter:
   ```bash
   python mowlang.py my_program.mow
   ```

2. Launch the interpreter interactively:
   ```bash
   python mowlang.py
   ```

---

## Command Reference

| **Command**     | **Description**                                                                                       |
|------------------|-------------------------------------------------------------------------------------------------------|
| `-->`            | Line comment. Everything after `-->` is ignored.                                                      |
| (empty line)     | Ignored by the interpreter.                                                                           |
| `PRINT`          | Prints a value or string. Accepts variables or string literals.                                       |
| `READ`           | Prompts user input and assigns it. Works with `SET`.                                                  |
| `SET`            | Declares a variable. Requires a name, type, and value.                                                |
| `LOAD`           | Retrieves a variable's value.                                                                         |
| `PAUSE`          | Pauses execution.                                                                                     |
| `TIME.START`     | Starts a timer.                                                                                       |
| `TIME.STOP`      | Stops the timer and logs the duration.                                                                |
| `TIME.SLEEP`     | Pauses execution for X seconds. Accepts float or integer.                                             |
| `TIME.STAMP`     | Returns current system timestamp.                                                                     |
| `TIME.NOW`       | Returns the current system time (formatted).                                                          |
| `IMPORT`         | Imports another `.mow` file. Can be string-literal or raw filename.                                   |
| `#DEFINE`        | Internal directive for configuration.                                                                 |
| `JUMP`           | Jumps to a previously defined `LABEL`.                                                                |
| `LABEL`          | Declares a label for use with `JUMP`.                                                                 |
| `FUNCTION`       | Begins a function definition. Requires a name and parameters.                                         |
| `END`            | Marks the end of a `FUNCTION`.                                                                         |
| `EXIT`           | Terminates program execution.                                                                          |

---

## #DEFINE Options

| **Directive**         | **Description**                                                                                     |
|-----------------------|-----------------------------------------------------------------------------------------------------|
| `NO_VERSION_WARNS`    | Suppresses version mismatch warnings.                                                               |
| `INPUT_PROMPT`        | Sets the default input prompt. Must be a string.                                                    |
| `REQUIRE_VERSION`     | Defines the minimum required interpreter version.                                                   |
| `DEBUG`               | Enables or disables debug mode. Accepts `ON`, `TRUE`.                                               |

---

## TIME Methods

| **Method**      | **Description**                                     | **Example**                 |
|------------------|-----------------------------------------------------|-----------------------------|
| `TIME.START`     | Starts the global timer.                            | `TIME.START`               |
| `TIME.STOP`      | Stops the timer and logs the duration.              | `TIME.STOP`                |
| `TIME.SLEEP X`   | Pauses execution for `X` seconds.                   | `TIME.SLEEP 1.5`           |
| `TIME.STAMP`     | Returns the current time in UNIX timestamp format.  | `TIME.STAMP`               |
| `TIME.NOW`       | Prints current time with optional format string.    | `TIME.NOW %H:%M:%S`        |

---

## File Importing

Use `IMPORT` to load another `.mow` file:

```mow
IMPORT "library"
```

It searches for `"library.mow"` and loads it before execution if the minimum version is lower than the file version.

---

## Variables

Declare a variable using `STORE`. Supported types: `int`, `float`, `bool`, `char`, `str`, `list`, `dict`.

```mow
STORE int x 10
SORE str name "John"
```

To load the variable later:

```mow
LOAD x
```

---

## Function Definition

Define a function with `FUNCTION` and end it with `END`.

```mow
FUNCTION sayHello name
PRINT "Hello,"
PRINT name
END
```

---

## Labels and Jumping

Define a label and jump to it:

```mow
LABEL start
PRINT "Running..."
JUMP start
```

Useful for loops or conditional jumps.

---

## Console Mode

Run `python mowlang.py` without arguments to enter interactive mode.

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
READ
STORE str $0
PRINT "Hello,"
PRINT name
TIME.SLEEP 2
EXIT
```

---

## Future Plans

- Function return values  
- Stack manipulation  
- Advanced conditional and looping structures  
- Native plugin system  

---

## Note

All `.mow` files may include a `filename:version` companion file. If missing, one will be created to match the current interpreter version.

---

## Versioning

The interpreter uses [PEP 440](https://www.python.org/dev/peps/pep-0440/) for semantic versioning. You can require specific versions with:

```mow
#DEFINE REQUIRE_VERSION 1.0.0
```