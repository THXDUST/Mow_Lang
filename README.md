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
| `DEFINE` | Defines variables that will be used by the interpreter. | `DEFINE StackDeprecationNoWarnings TRUE; // Using Stack will not produce Pending Deprecation Warning` |
| `READ` | Simply reads user inputs. | `READ([optional: input prompt])` <br /> `STR input = READ("~> ");` |

Types <br />
`INT`, `FLOAT`, `STR`, `BOOL`, `LIST`, `DICT`, `CHAR`, `VOID`.

Expressions & operators <br>
Arithmetic: `+ - * / ^` (power). Relational: `< > <= >= == !=`. Logic: `&& || !`. String concatenation: `+` when operand is string.

Functions <br>
Syntax: `FUNCTION [RET_TYPE] [STATIC/CONST] name(param: TYPE, ...) { body }`. Params may have defaults. Use `RETURN(value)` for non-`VOID` returns. Essentially, using STATIC or CONST will produce the same result on functions.

Classes / OOP <br>
Declare properties and methods inside `CLASS`. Inside methods `this` refers to the instance. Property access uses dot `this.prop` or `instance.prop`.

List literals <br>
Use `[a, b, c]` - elements can be expressions.

Comments <br>
Line: `// comment`. Block: `/* comment */`.

Error reporting & debugging <br>
Interpreter throws syntax/name/type errors with location info.
<br />

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

Contributing & tests <br />
- Add `.mow` examples and run `mowlang.py` for manual tests.
<br /><br />

# Type Methods in MowLang

This documentation describes all native methods available for data types in MowLang.

## STRING (STR)

Available string methods:

| Method | Parameters | Description |
|--------|-----------|-----------|
| `UPPER()` | - | Converts the string to uppercase |
| `LOWER()` | - | Converts the string to lowercase |
| `LENGTH()` | - | Returns the string length |
| `TRIM()` | - | Removes whitespace from the beginning and end |
| `STRIP()` | - | Alias for `TRIM()` |
| `REVERSE()` | - | Returns the reversed string |
| `REPLACE(old, new)` | old (STR), new (STR) | Replaces occurrences of `old` with `new` |
| `SPLIT(sep)` | sep (STR, optional) | Splits the string by a separator (default: space) |
| `STARTSWITH(prefix)` | prefix (STR) | Returns 1 if it starts with `prefix`, otherwise 0 |
| `ENDSWITH(suffix)` | suffix (STR) | Returns 1 if it ends with `suffix`, otherwise 0 |
| `CONTAINS(substr)` | substr (STR) | Returns 1 if it contains `substr`, otherwise 0 |
| `FIND(substr)` | substr (STR) | Returns the index of `substr` or -1 if not found |
| `COUNT(substr)` | substr (STR) | Counts occurrences of `substr` |
| `ISDIGIT()` | - | Returns 1 if all characters are digits, otherwise 0 |
| `ISALPHA()` | - | Returns 1 if all characters are letters, otherwise 0 |
| `ISALNUM()` | - | Returns 1 if alphanumeric, otherwise 0 |
| `ISSPACE()` | - | Returns 1 if all characters are whitespace, otherwise 0 |

### Examples

```mow
STR text = "Hello World";
PRINTL(text.UPPER());
PRINTL(text.LOWER());
PRINTL(text.LENGTH());
PRINTL(text.REVERSE());
PRINTL(text.STARTSWITH("Hello"));
PRINTL(text.REPLACE("World", "MowLang"));
```

## LIST

Available list methods:

| Method | Parameters | Description |
|--------|-----------|-----------|
| `APPEND(item)` | item (any type) | Adds an item to the end of the list |
| `POP(idx)` | idx (INT, optional, default: -1) | Removes and returns the item at index |
| `REMOVE(item)` | item (any type) | Removes the first occurrence of the item |
| `INSERT(idx, item)` | idx (INT), item (any type) | Inserts the item at the index |
| `CLEAR()` | - | Removes all items |
| `LENGTH()` | - | Returns the number of items |
| `REVERSE()` | - | Reverses the order of items |
| `SORT()` | - | Sorts the items |
| `INDEX(item)` | item (any type) | Returns the index of the first occurrence |
| `COUNT(item)` | item (any type) | Counts occurrences of the item |
| `COPY()` | - | Returns a copy of the list |
| `CONTAINS(item)` | item (any type) | Returns 1 if it contains the item, otherwise 0 |

### Examples

```mow
LIST arr = [3, 1, 4, 1, 5];
PRINTL(arr.LENGTH());
arr.APPEND(9);
PRINTL(arr.LENGTH());
PRINTL(arr.CONTAINS(3));
PRINTL(arr.COUNT(1));
PRINTL(arr.INDEX(4));
```

## DICT (Dictionary)

Available dictionary methods:

| Method | Parameters | Description |
|--------|-----------|-----------|
| `KEYS()` | - | Returns a list of all keys |
| `VALUES()` | - | Returns a list of all values |
| `ITEMS()` | - | Returns a list of (key, value) pairs |
| `GET(key, default)` | key (STR), default (optional) | Returns the value for the key or a default |
| `POP(key, default)` | key (STR), default (optional) | Removes and returns the value for the key |
| `CLEAR()` | - | Removes all pairs |
| `UPDATE(other)` | other (DICT) | Adds or updates pairs from another dictionary |
| `LENGTH()` | - | Returns the number of pairs |
| `COPY()` | - | Returns a copy of the dictionary |
| `CONTAINS(key)` | key (STR) | Returns 1 if the key exists, otherwise 0 |
| `REMOVE(key)` | key (STR) | Removes the key and returns its value |

### Examples

```mow
DICT person = {"name": "João", "age": "25"};
PRINTL(person.LENGTH());
PRINTL(person.CONTAINS("name"));
PRINTL(person.GET("age"));
```

## INT (Integer)

Available integer methods:

| Method | Parameters | Description |
|--------|-----------|-----------|
| `ABS()` | - | Returns the absolute value |
| `STR()` | - | Converts to string |
| `FLOAT()` | - | Converts to float |
| `BIT_LENGTH()` | - | Returns the number of bits |
| `TO_HEX()` | - | Converts to hexadecimal |
| `TO_BIN()` | - | Converts to binary |
| `TO_OCT()` | - | Converts to octal |

### Examples

```mow
INT num = -42;
PRINTL((-42).ABS());
PRINTL((255).TO_HEX());
PRINTL((8).TO_BIN());
PRINTL((16).TO_OCT());
```

## FLOAT

Available float methods:

| Method | Parameters | Description |
|--------|-----------|-----------|
| `ABS()` | - | Returns the absolute value |
| `STR()` | - | Converts to string |
| `INT()` | - | Converts to integer (truncates) |
| `ROUND(digits)` | digits (INT, optional) | Rounds to a number of decimal places |
| `CEIL()` | - | Rounds up |
| `FLOOR()` | - | Rounds down |

### Examples

```mow
FLOAT val = 3.14159;
PRINTL((-3.14).ABS());
PRINTL(val.ROUND(2));
PRINTL(val.INT());
```

## BOOL (Boolean)

Available boolean methods:

| Method | Parameters | Description |
|--------|-----------|-----------|
| `STR()` | - | Converts to string ("TRUE" or "FALSE") |
| `INT()` | - | Converts to integer (1 or 0) |

### Examples

```mow
INT flag = 1;
PRINTL((flag).INT());
PRINTL((flag).STR());
```

## Important Notes

1. **Parentheses on Literals**  
   To call methods on literals or expressions, always use `(value).method()`.

2. **Variables**  
   Methods can be called directly on variables.

3. **Return Values**  
   Methods return values that can be assigned to variables or used in expressions.

4. **List and Dictionary Mutations**  
   Some methods modify the original structure.

5. **Errors**  
   Using non-existent methods or invalid arguments will raise a runtime error.