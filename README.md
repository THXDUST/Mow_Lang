# My Own Programming Language

I was bored and made this Python interpreter.


For better understanding, imagine this lang as a list

_________________________________________________________________

a few changes since last update: <br />-- the PRINT command NEEDS at least one argument and double quotes for strings, use any number to print the top value of the stack; <br />-- Console mode added, where you can try the commands before trying to build something on your own; <br />-- A simple error handling for incorrect commands, valid for both Console mode and File Reader mode; <br />-- New commands: HELP and RANDOM.

To run any code made for this lang, open the archive with the interpreter `mowlang.exe` or open the `mowlang.exe` and run your code directly there

| Key Words    | Explanation                                                            | Example                                                      |
|--------------|------------------------------------------------------------------------|--------------------------------------------------------------|
| -->          | It is for doing a comment                                              | `--> should skip this line`                                  |
| `empty line` | The interpreter just skips all of the empty lines                      | `    ` skips this line                                       |
| PUSH         | This key word insert a value at the top of the list                    | `PUSH 5` insert the 5 as the first element                   |
| POP          | Removes an element out of the top of the list                          | `POP` using the example above, it should remove the 5        |
| ADD          | Adds two numbers together and insert the result at the top of the list | `ADD` should do a + b and return c, <br /> `PUSH 5` <br /> `PUSH 5` <br /> `ADD` <br /> it should return 10           |
| SUB          | Subtracts two numbers and insert the result at the top of the list     | `SUB` should do a - b and return c, <br /> `PUSH 9` <br /> `PUSH 10` <br /> `SUB` <br /> it should return 1           |
| MUL          | Multiplies two numbers and insert the result at the top of the list    | `MUL` should do a * b and return c, <br /> `PUSH 2` <br /> `PUSH 5` <br /> `MUL` <br /> it should return 10           |
| DIV          | Divides two numbers and insert the result at the top of the list       | `DIV` should do a // b and return c, <br /> `PUSH 3` <br /> `PUSH 9` <br /> `DIV` <br /> it should return 3           |
| PRINT        | Prints a string or variable to the console                             | `PRINT 0` should print the top value of the list, `PRINT "Hello World"` should print Hello World                      |
| READ         | Asks the user for an integer input                                     | `READ` should wait for the user to input an integer, any positive or negative number                                  |
| JUMP.EQ.0    | Jumps to a label if the top number on the stack equals zero            | `JUMP.EQ.0 L1` should jump to the code block labeled `L1` if the top value is 0                                       |
| JUMP.GT.0    | Jumps to a label if the top number on the stack is greater than zero   | `JUMP.GT.0 L1` should jump to the code block labeled `L1` if the top value is grater than 0                           |
| JUMP.LT.0    | Jumps to a label if the top number on the stack is less than zero      | `JUMP.LT.0 L1` should jump to the code block labeled `L1` if the top value is less than 0                             |
| JUMP         | Jumps unconditionally to a label                                       | `JUMP L1` should jump to the code block labeled `L1`         |
| DUP          | Duplicates the top value of the stack                                  | `DUP` if the stack contains [5], results in [5, 5]           |
| SWAP         | Swaps the two top values of the stack                                  | `SWAP` if the stack contains [2, 3], results in [3, 2]       |
| EXIT         | Terminates the execution of the program                                | `EXIT` stops the program                                     |
| MOD          | Computes the modulus of two numbers and inserts the result at the top of the stack       | `MOD` should do a % b and return c, <br /> `PUSH 10` <br /> `PUSH 3` <br /> `MOD` <br /> it should return 1|
| EXP          | Raises one number to the power of another and inserts the result at the top of the stack | `EXP` should do a ** b and return c, <br /> `PUSH 2` <br /> `PUSH 3` <br /> `EXP` <br /> it should return 8|
| STORE        | Store the top value in a variable                                      | `STORE var` it will store the top value at "var", <br /> `PUSH 3` <br /> `STORE var`, <br /> `PUSH 1`, <br /> `LOAD war` <br /> it should return 3 |
| LOAD         | Load a value from a variable to the top                                | `LOAD var` using the example above, at the `LOAD var`, it gets the value of the variable "var"                        |
| PRINT.VARS   | Prints all of the current saved variables                              | `PRINT.VARS` prints all of the current saved variables, <br /> `PUSH 1` <br /> `STORE a` <br /> `PUSH 5` <br /> `STORE b` <br /> `PRINT.VARS` <br /> it should return: <br /> `Variables:` <br /> `a = 1` <br /> `b = 5` |
| RANDOM       | Generates a random number using 0 to 2 arguments and inserts it at the top of the stack  | `RANDOM` will generate a random number between 0 and 100, `RANDOM 50` will generate a random number between 0 and 50, `RANDOM 10 50` wil generate a random number between 10 and 50 |
| HELP         | Prints a help message explaining all of the available commands         | `HELP` prints a help message explaining all of the available commands  |


