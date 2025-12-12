# My Own Programming Language

I created this Python-based interpreter as a fun project.

For better understanding, think of this language as operating like a list.

---

## Recent Updates
- You have the option to allocate as many bytes as needed, but it MUST allocate at least 1 byte.
- New commands: `SWAP`, `DUP`, `PRINT.STACK`, `ROT`, `CLEAR`, `ALLOC`, `TIME`, `TIME.SINCE`, `WAIT` and `ALLOC.DESTROY`.

### Running Your Code
To execute a program written in this language, either:
1. Open the source file with the interpreter (`mowlang.exe`).
2. Launch `mowlang.exe` directly and enter your code interactively.

---

## Commands & Descriptions

| **Keyword**   | **Description**                                                          | **Example**                                                  |
|--------------|--------------------------------------------------------------------------|--------------------------------------------------------------|
| `-->`        | Used for writing comments. The interpreter ignores these lines.         | `--> This is a comment`                                      |
| _(empty line)_ | The interpreter skips all empty lines.                                  | _(blank line)_                                              |
| `PUSH`       | Inserts a value at the top of the stack.                                | `PUSH 5` (places `5` at the top of the stack)               |
| `POP`        | Removes the top element from the stack.                                 | `POP` (removes the last pushed value)                       |
| `ADD`        | Pops two numbers, adds them, and pushes the result.                     | `PUSH 5` <br> `PUSH 5` <br> `ADD` (returns `10`)            |
| `SUB`        | Pops two numbers, subtracts them, and pushes the result.                | `PUSH 9` <br> `PUSH 10` <br> `SUB` (returns `1`)            |
| `MUL`        | Pops two numbers, multiplies them, and pushes the result.               | `PUSH 2` <br> `PUSH 5` <br> `MUL` (returns `10`)            |
| `DIV`        | Pops two numbers, performs integer division, and pushes the result.     | `PUSH 3` <br> `PUSH 9` <br> `DIV` (returns `3`)             |
| `PRINT`      | Prints a string or the top value of the stack.                          | `PRINT 0` (prints the top stack value) <br> `PRINT "Hello"` |
| `READ`       | Prompts the user for an integer input and pushes it onto the stack.     | `READ` (user enters a number)                               |
| `JUMP.EQ.0`  | Jumps to a label if the top stack value equals zero.                    | `JUMP.EQ.0 L1`                                              |
| `JUMP.GT.0`  | Jumps to a label if the top stack value is greater than zero.           | `JUMP.GT.0 L1`                                              |
| `JUMP.LT.0`  | Jumps to a label if the top stack value is less than zero.              | `JUMP.LT.0 L1`                                              |
| `JUMP`       | Unconditionally jumps to a label.                                      | `JUMP L1`                                                   |
| `DUP`        | Duplicates the top value of the stack.                                 | `DUP` (stack `[5]` becomes `[5, 5]`)                        |
| `SWAP`       | Swaps the two top values on the stack.                                | `SWAP` (stack `[2, 3]` becomes `[3, 2]`)                    |
| `EXIT`       | Terminates program execution.                                         | `EXIT`                                                      |
| `MOD`        | Pops two numbers, computes the modulus, and pushes the result.       | `PUSH 10` <br> `PUSH 3` <br> `MOD` (returns `1`)            |
| `EXP`        | Pops two numbers, raises the second to the power of the first, and pushes the result. | `PUSH 2` <br> `PUSH 3` <br> `EXP` (returns `8`) |
| `STORE`      | Stores the top stack value in a variable.                             | `PUSH 3` <br> `STORE var`                                   |
| `LOAD`       | Loads a variable’s value onto the stack.                              | `LOAD var`                                                  |
| `PRINT.VARS` | Displays all currently stored variables.                              | `PRINT.VARS`                                                |
| `RANDOM`     | Generates a random number based on given arguments.                   | `RANDOM` (0-100) <br> `RANDOM 50` (0-50) <br> `RANDOM 10 50` (10-50) |
| `HELP`       | Displays a help message listing all available commands.              | `HELP`                                                      |
| `ALLOC`      | Allocates memory of a given size.                                    | `ALLOC 5`                                                   |
| `ROT`        | Rotates the top three stack elements.                                | `PUSH 1` <br> `PUSH 2` <br> `PUSH 3` <br> `ROT` (becomes `[3, 2, 1]`) |
| `CLEAR`      | Clears the entire stack.                                             | `CLEAR`                                                     |
| `PRINT.STACK`| Prints the current stack contents.                                   | `PRINT.STACK`                                               |
| `TIME`       | Prints the current system time.                                      | `TIME UTC` or `TIME LOCAL`                                  |
| `TIME.SINCE` | Prints the elapsed time since the program started.                   | `TIME.SINCE` (returns elapsed seconds)                      |
| `WAIT`       | Pauses execution for a specified time.                               | `WAIT 5` (pauses for 5 seconds)                             |
| `ALLOC.DESTROY` | Destroys an allocated memory block.                              | `ALLOC.DESTROY`                                             |

---

## Command Arguments

| **Keyword**   | **Arguments** |
|--------------|--------------|
| `PUSH`       | An integer (e.g., `5`) |
| `PRINT`      | A string (`"Hello"`) or an integer (`1`, prints the stack’s top value) |
| `JUMP.EQ.0`  | A label (e.g., `L1`) |
| `JUMP.GT.0`  | A label (e.g., `L1`) |
| `JUMP.LT.0`  | A label (e.g., `L1`) |
| `JUMP`       | A label (e.g., `L1`) |
| `STORE`      | A variable name (e.g., `var`) |
| `LOAD`       | A variable name (e.g., `var`) |
| `RANDOM`     | _(Optional)_ An integer range (`nothing` or `50` or `10 50`) |
| `ALLOC`      | An integer defining memory size (e.g., `5`) |
| `TIME`       | `UTC` or `LOCAL` to define the timezone. Additional options: `FULL`, `YEAR`, `MONTH`, `DAY`, `HOUR`, `MINUTE`, `SECOND`, `MICROSECOND` |
| `WAIT`       | An integer or float defining the delay in seconds (e.g., `5` or `1.7`) |