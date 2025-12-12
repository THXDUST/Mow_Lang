# MowLang - Language and Interpreter

**DISCLAIMER: I DID NOT ADAPTED THE OLD EXAMPLES TO THIS NEW REMAKE, INSTEAD, THEY ARE ADAPTED TO A PAST REMAKE I DID THAT WAS NOT POSTED ON GITHUB DUE TO LOTS OF ERRORS AND INCONSISTENCIES THAT ARE NOT PRESENT ON THE CURRENT REMAKE, THIS PAST REMAKE VERSION IS PRESENT UNDER OldVersions/25-09-16.**
<br /><br />
This repository contains the Python interpreter for the MowLang language. This README provides a concise reference with commands, short descriptions and syntax examples, followed by a table of command arguments.

Quick start<br />
Open a terminal in the project folder.<br />
Run:
```powershell
python mowlang.py [file path] [--debug]
```


# MowLang - Cheat-sheet & Quick Reference

This file is a compact cheat-sheet for MowLang. It focuses on commands, syntax, types and short examples.<br />

Commands - description and syntax
| Keyword | Description | Syntax / Example |
|---|---|---|
| `PRINT` / `PRINTL` | Evaluate expression and print it; `PRINTL` appends newline. | `PRINT("Hello");` `PRINTL(var);` |
| `EXIT` | Exit program with numeric code. | `EXIT(0);` |
| `LABEL` | Create named label for `GOTO`. | `LABEL("start");` |
| `GOTO` | Jump to a label. | `GOTO("start");` |
| `FUNCTION` | Define function. Optional return type. | `FUNCTION VOID foo(a: INT) { ... }` |
| `RETURN` | Return a value from a function. | `RETURN(42);` |
| `CLASS` | Declare class with typed properties/methods. | `CLASS Person { STR name; INT age; FUNCTION VOID say() { ... } }` |
| Instance creation | Declare an instance of a class. | `Person john;` then `john.setName("X");` |
| Member access `.` | Access/read/write properties or call methods. | `john.name` `john.setAge(30);` |
| Variable declaration | Typed variable declaration and optional init. | `INT x;` `STR s = "hi";` |
| `FOR` | Runs a loop, can be passed a list or dict to iterate over it. | `FOR (INT x = 0; x < 10; x = x + 1) { body }` <br /> `FOR (CONST item : LIST([1, 2, 3])) { body }` <br /> `FOR (CONST key, CONST value : DICT({"key1": "value1", "key2": 2})) { body }`|
| `WHILE` | Runs a loop while a condition is true. | `WHILE (TRUE) { body }` |
| `BREAK` | Stops any loop currently running. | `BREAK` |
| `CONTINUE` | Proceed to the next loop iteration. | `CONTINUE` |
| `BREAKPOINT` | Adds a breakpoint to stop the program, helps debugging and shows variables, classes, labels and functions. | `BREAKPOINT()` |

Types <br />
`INT`, `FLOAT`, `STR`, `BOOL`, `LIST`, `DICT`, `CHAR`, `VOID`.

Expressions & operators <br>
Arithmetic: `+ - * / ^` (power). Relational: `< > <= >= == !=`. Logic: `&& || !`. String concatenation: `+` when operand is string.

Functions <br>
Syntax: `FUNCTION [RET_TYPE] name(param: TYPE, ...) { body }`. Params may have defaults. Use `RETURN(value)` for non-`VOID` returns.

Classes / OOP <br>
Declare properties and methods inside `CLASS`. Inside methods `this` refers to the instance. Property access uses dot `this.prop` or `instance.prop`.

List literals <br>
Use `[a, b, c]` - elements can be expressions.

Comments <br>
Line: `// comment`. Block: `/* comment */`.

Error reporting & debugging <br>
Interpreter throws syntax/name/type errors with location info.

<br /><br />
## Examples (ErrorsTesting2.mow)
```
CLASS Person {
    STR name;
    INT age;
    FUNCTION VOID sayHello() {
        PRINTL("Hello, my name is " + this.name);
    }

    FUNCTION VOID setAge(newAge: INT) {
        this.age = newAge;
    }

    FUNCTION VOID setName(newName: STR) {
        this.name = newName;
    }
}

Person john; // Instanciate Person
john.setName("John"); // Define instance name to "John"
john.sayHello(); // Prints: Hello, my name is John
john.setAge(31); // Define instance age to 31
PRINTL(john); // Prints: Instance of Person (name=John, age=31)
```
More can be found through example files at examples directory.

<br />
Contributing & tests <br />
- Add `.mow` examples and run `mowlang.py` for manual tests.