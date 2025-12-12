# MowLang - Language and Interpreter

This repository contains the Python interpreter for the MowLang language. This README provides a concise reference with commands, short descriptions and syntax examples, followed by a table of command arguments.

Quick start<br />
Open a terminal in the project folder.<br />
Run:
```powershell
python mowlangNEW.py [file path]
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
Interpreter throws syntax/name/type errors with location info. Developer helpers available in `MowUtils.py` / `MowImports.py` (e.g., `MI.PrintDict`).

Command argument reference
| Keyword | Arguments |
|---|---|
| `PRINT`, `PRINTL` | Any expression; call with parentheses and end `;` |
| `EXIT` | Numeric literal or integer variable |
| `LABEL` | String label name (or expression resolving to string) |
| `GOTO` | Label name string |
| `FUNCTION` | `FUNCTION [RET_TYPE] name(param: TYPE, ...) { ... }` |
| `RETURN` | Expression matching declared return type |
| `CLASS` | `CLASS Name { <properties>; <methods> }` |
| Instance creation | `ClassName instanceName;` |
| Variable declaration | `TYPE name;` or `TYPE name = expr;` |

<br /> <br /> <br />
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
- Add `.mow` examples and run `mowlangNEW.py` for manual tests.