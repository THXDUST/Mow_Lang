import codecs, re, os
from copy import deepcopy
from MowErrors import ThrowNameError, ThrowRuntimeError, ThrowNotImplementedError, ThrowSyntaxError, ThrowTypeError, ThrowValueError
from MowTypes import Token, Variable, Function, Class, Label
from MowMethods import BuiltInMethods

datatypes = ["VOID", "LIST", "DICT", "INT", "FLOAT", "BOOL", "STR", "CHAR"]

keywords = ["PRINT", "PRINTL", "EXIT", "LABEL", "FUNCTION", "GOTO", "RETURN", "CLASS", "FOR", "WHILE", "BREAK", "CONTINUE", "BREAKPOINT", "DEFINE", "READ"]
#              0        1        2        3         4         5        6         7       8       9       10         11           12          13       14

DEPTH = 0

def CallFunction(func_name: str, args: list, token: Token, Globals: dict) -> any:
    """
    Helper to call a function from within EvalExpr and return result.
    This is needed because we need proper state management.
    """
    if func_name not in Globals.get("FUNCTIONS", {}):
        ThrowNameError(f"Function '{func_name}' not defined", token, Globals)
    
    func = Globals["FUNCTIONS"][func_name]
    
    # Save old state
    old_globals_vars = deepcopy(Globals["VARIABLES"])
    old_in_func = Globals.get("_in_function", False)
    Globals["VARIABLES"] = {}
    retval = None
    restore_state = True  # Track whether we should restore state
    
    try:
        # Set up parameters
        for idx, p in enumerate(func.parameters):
            pname = p.name
            ptype = p.type or "*"
            
            if idx < len(args):
                aval = args[idx]
            else:
                if hasattr(p, "default") and p.default is not None:
                    aval = p.default
                else:
                    ThrowRuntimeError(f"Missing argument for parameter '{pname}'", token, Globals)
            
            SetVariable(Globals, pname, aval, False, ptype, token.line, token.char_pos, token)
        
        # Execute function body
        body_tokens = func.body.copy()
        body_tokens.append(Token("EOF", None, token.line, token.char_pos))
        
        Globals["_in_function"] = True
        try:
            # We need to call the Run function from mowlang
            # But we have to be careful to use a fresh context
            # Import here to avoid circular imports at module load time
            import sys
            if 'mowlang' in sys.modules:
                mowlang = sys.modules['mowlang']
                # Call Run with custom GLOBALS to use our modified dictionary
                mowlang.Run(body_tokens, custom_GLOBALS=Globals)
            else:
                # Fallback if mowlang not yet loaded
                from mowlang import Run
                Run(body_tokens, custom_GLOBALS=Globals)
        except Exception as exc:
            # Check if it's a ReturnException
            if type(exc).__name__ == "ReturnException":
                retval = getattr(exc, 'value', None)
                restore_state = True  # Still restore after function returns
            else:
                # Some other error - restore globals and re-raise
                restore_state = True
                raise
    finally:
        # Always restore globals after function execution
        if restore_state:
            Globals["VARIABLES"] = old_globals_vars
            Globals["_in_function"] = old_in_func
    
    return retval

def EvalExpr(Tokens: list[Token], Globals: dict, Lines: list[str]) -> any:
    if len(Tokens) == 0:
        return None
    
    # --- Pre-process: Handle function calls in expressions (e.g testB(6)) ---
    # Build a new list to avoid modifying while iterating
    new_tokens = []
    i = 0
    while i < len(Tokens):
        # Check for function call pattern: identifier(...)
        if (i < len(Tokens) - 1 and 
            Tokens[i].type == "identifier" and 
            Tokens[i + 1].type == "left_parenthesis" and
            Tokens[i].aux in Globals.get("FUNCTIONS", {})):
            
            func_name_token = Tokens[i]
            func_name = func_name_token.aux
            i += 2  # Skip identifier and left_paren
            
            # Extract arguments until matching right_paren
            args_tokens = []
            paren_depth = 1
            while i < len(Tokens) and paren_depth > 0:
                if Tokens[i].type == "left_parenthesis":
                    paren_depth += 1
                    args_tokens.append(Tokens[i])
                elif Tokens[i].type == "right_parenthesis":
                    paren_depth -= 1
                    if paren_depth > 0:
                        args_tokens.append(Tokens[i])
                else:
                    args_tokens.append(Tokens[i])
                i += 1
            
            # Parse arguments
            args = []
            if args_tokens:
                current_arg = []
                arg_depth = 0
                for tok in args_tokens:
                    if tok.type in ["left_parenthesis", "left_bracket", "left_key"]:
                        arg_depth += 1
                        current_arg.append(tok)
                    elif tok.type in ["right_parenthesis", "right_bracket", "right_key"]:
                        arg_depth -= 1
                        current_arg.append(tok)
                    elif tok.type == "comma" and arg_depth == 0:
                        if current_arg:
                            args.append(EvalExpr(current_arg, Globals, Lines))
                        current_arg = []
                    else:
                        current_arg.append(tok)
                if current_arg:
                    args.append(EvalExpr(current_arg, Globals, Lines))
            
            # Call function
            retval = CallFunction(func_name, args, func_name_token, Globals)
            
            # Add result token
            if isinstance(retval, str):
                new_tokens.append(Token("string_literal", retval, func_name_token.line, func_name_token.char_pos))
            elif isinstance(retval, (int, float)):
                new_tokens.append(Token("numeric_literal", retval, func_name_token.line, func_name_token.char_pos))
            elif isinstance(retval, list):
                new_tokens.append(Token("list_literal", retval, func_name_token.line, func_name_token.char_pos))
            elif isinstance(retval, dict):
                new_tokens.append(Token("dict_literal", retval, func_name_token.line, func_name_token.char_pos))
            elif retval is None:
                new_tokens.append(Token("empty", None, func_name_token.line, func_name_token.char_pos))
            else:
                new_tokens.append(Token("identifier", str(retval), func_name_token.line, func_name_token.char_pos, 0, retval))
        else:
            new_tokens.append(Tokens[i])
            i += 1
    
    Tokens[:] = new_tokens  # Update original list in place
    
    # --- Pre-process: Handle method calls on literals/expressions ---
    processed_any = True
    while processed_any:
        processed_any = False
        i = 0
        while i < len(Tokens) - 3:
            can_have_method = False
            base_start = i
            base_end = i

            if (Tokens[i].type in ["string_literal", "numeric_literal", "list_literal", "dict_literal", "identifier"] and
                i + 1 < len(Tokens) and Tokens[i + 1].type == "dot"):
                can_have_method = True
                base_end = i
                
            elif Tokens[i].type == "left_parenthesis":
                paren_depth = 1
                j = i + 1
                while j < len(Tokens) and paren_depth > 0:
                    if Tokens[j].type == "left_parenthesis":
                        paren_depth += 1
                    elif Tokens[j].type == "right_parenthesis":
                        paren_depth -= 1
                    j += 1
                
                if (paren_depth == 0 and j < len(Tokens) and Tokens[j].type == "dot"):
                    can_have_method = True
                    base_end = j - 1
            
            if (can_have_method and base_end + 1 < len(Tokens) and Tokens[base_end + 1].type == "dot" and
                base_end + 2 < len(Tokens) and Tokens[base_end + 2].type == "identifier" and
                base_end + 3 < len(Tokens) and Tokens[base_end + 3].type == "left_parenthesis"):
                
                method_name = Tokens[base_end + 2].aux
                paren_token = Tokens[base_end + 3]
                
                if Tokens[i].type == "left_parenthesis":
                    expr_tokens = Tokens[i + 1:base_end]
                    base_value = EvalExpr(expr_tokens, Globals, Lines)
                    base_token = Tokens[i]
                else:
                    base_token = Tokens[i]
                    if base_token.type == "identifier":
                        if base_token.value in Globals["VARIABLES"]:
                            base_value = Globals["VARIABLES"][base_token.value].value
                        else:
                            ThrowNameError(f"Variable '{base_token.value}' not defined", base_token, Globals)
                    else:
                        base_value = base_token.value
                
                args_tokens = []
                j = base_end + 4
                paren_depth = 1
                while j < len(Tokens) and paren_depth > 0:
                    if Tokens[j].type == "left_parenthesis":
                        paren_depth += 1
                        args_tokens.append(Tokens[j])
                    elif Tokens[j].type == "right_parenthesis":
                        paren_depth -= 1
                        if paren_depth == 0:
                            break
                        args_tokens.append(Tokens[j])
                    else:
                        args_tokens.append(Tokens[j])
                    j += 1
                
                args = []
                if args_tokens:
                    current_arg = []
                    arg_depth = 0
                    for tok in args_tokens:
                        if tok.type in ["left_parenthesis", "left_bracket", "left_key"]:
                            arg_depth += 1
                            current_arg.append(tok)
                        elif tok.type in ["right_parenthesis", "right_bracket", "right_key"]:
                            arg_depth -= 1
                            current_arg.append(tok)
                        elif tok.type == "comma" and arg_depth == 0:
                            if current_arg:
                                args.append(EvalExpr(current_arg, Globals, Lines))
                            current_arg = []
                        else:
                            current_arg.append(tok)
                    if current_arg:
                        args.append(EvalExpr(current_arg, Globals, Lines))
                
                result = BuiltInMethods.call_method(base_value, method_name, args, base_token, Globals)
                
                num_to_remove = j - i + 1
                for _ in range(num_to_remove):
                    if i < len(Tokens):
                        Tokens.pop(i)
                
                # Insert result as new token
                if isinstance(result, str):
                    Tokens.insert(i, Token("string_literal", result, base_token.line, base_token.char_pos))
                elif isinstance(result, (int, float)):
                    Tokens.insert(i, Token("numeric_literal", result, base_token.line, base_token.char_pos))
                elif isinstance(result, list):
                    Tokens.insert(i, Token("list_literal", result, base_token.line, base_token.char_pos))
                elif isinstance(result, dict):
                    Tokens.insert(i, Token("dict_literal", result, base_token.line, base_token.char_pos))
                elif result is None:
                    Tokens.insert(i, Token("empty", None, base_token.line, base_token.char_pos))
                else:
                    Tokens.insert(i, Token("identifier", str(result), base_token.line, base_token.char_pos, 0, result))
                
                processed_any = True
                break
            
            i += 1
    
    resolved_tokens = []
    i = 0
    while i < len(Tokens):
        t = Tokens[i]

        if (
            t.type == "identifier"
            and i + 2 < len(Tokens)
            and Tokens[i + 1].type == "dot"
            and Tokens[i + 2].type == "identifier"
        ):
            base_name = t.value
            member_name = Tokens[i + 2].value

            if base_name in Globals["VARIABLES"]:
                base_val = Globals["VARIABLES"][base_name].value
                
                if hasattr(base_val, "attributes") and member_name in base_val.attributes:
                    val = base_val.attributes[member_name]
                    if isinstance(val, str):
                        resolved_tokens.append(Token("string_literal", val, t.line, t.char_pos))
                    elif isinstance(val, (int, float)):
                        resolved_tokens.append(Token("numeric_literal", val, t.line, t.char_pos))
                    elif isinstance(val, list):
                        resolved_tokens.append(Token("list_literal", val, t.line, t.char_pos))
                    elif isinstance(val, dict):
                        resolved_tokens.append(Token("dict_literal", val, t.line, t.char_pos))
                    elif val is None:
                        resolved_tokens.append(Token("empty", None, t.line, t.char_pos))
                    else:
                        resolved_tokens.append(Token("identifier", val, t.line, t.char_pos, 0, val))
                    
                else:
                    ThrowNameError(f"Property '{member_name}' not found in instance '{base_name}'", Tokens[i + 2], Globals)
            else:
                ThrowNameError(f"Variable '{base_name}' not found", t, Globals)

            i += 3
            continue

        resolved_tokens.append(t)
        i += 1

    Tokens = resolved_tokens

    while Tokens and Tokens[0].type == "whitespace":
        Tokens.pop(0)
    while Tokens and Tokens[-1].type == "whitespace":
        Tokens.pop()

    if Tokens and Tokens[0].type == "left_bracket" and Tokens[-1].type == "right_bracket":
        elements = []
        current_element = []
        depth = 0

        for token in Tokens[1:-1]:
            if token.type == "comma" and depth == 0:
                if current_element:
                    elements.append(EvalExpr(current_element, Globals, Lines))
                    current_element = []
            else:
                if token.type == "left_bracket":
                    depth += 1
                elif token.type == "right_bracket":
                    depth -= 1
                current_element.append(token)

        if current_element:
            elements.append(EvalExpr(current_element, Globals, Lines))

        return elements

    if Tokens and Tokens[0].type == "left_key" and Tokens[-1].type == "right_key":
        items = []
        current_key = []
        current_value = []
        reading_value = False
        depth = 0

        for token in Tokens[1:-1]:
            if token.type == "left_key" or token.type == "left_bracket":
                depth += 1
            elif token.type == "right_key" or token.type == "right_bracket":
                depth -= 1

            if depth == 0 and token.type == "colon" and not reading_value:
                reading_value = True
                continue

            if depth == 0 and token.type == "comma":
                if current_key and current_value:
                    k = EvalExpr(current_key, Globals, Lines)
                    v = EvalExpr(current_value, Globals, Lines)
                    items.append((k, v))
                current_key = []
                current_value = []
                reading_value = False
                continue

            if not reading_value:
                current_key.append(token)
            else:
                current_value.append(token)

        if current_key:
            if not current_value:
                ThrowRuntimeError("Invalid expression", Tokens[0], Globals)
            k = EvalExpr(current_key, Globals, Lines)
            v = EvalExpr(current_value, Globals, Lines)
            items.append((k, v))

        result_dict = {}
        for k, v in items:
            if isinstance(k, (int, float, bool)):
                k = str(k)
            elif k is None:
                k = "None"
            elif not isinstance(k, str):
                try:
                    k = str(k)
                except Exception:
                    ThrowTypeError("Invalid dict key type", Tokens[0], Globals)
            result_dict[k] = v

        return result_dict

    if Tokens and Tokens[0].type == "keyword" and Tokens[0].aux == "READ":
        p_idx = None
        for idx, t in enumerate(Tokens[1:], start=1):
            if t.type == "left_parenthesis":
                p_idx = idx
                break
        if p_idx is not None:
            depth = 0
            q_idx = None
            for j in range(p_idx, len(Tokens)):
                tt = Tokens[j]
                if tt.type == "left_parenthesis":
                    depth += 1
                elif tt.type == "right_parenthesis":
                    depth -= 1
                    if depth == 0:
                        q_idx = j
                        break
            if q_idx is not None:
                inner = Tokens[p_idx+1:q_idx]
                if not inner:
                    prompt = ""
                else:
                    try:
                        prompt_val = EvalExpr(inner, Globals, Lines)
                    except Exception as e:
                        ThrowRuntimeError(str(e), Tokens[0], Globals)

                    prompt, ok = Convert(prompt_val, "STR", True, Tokens[0].line, Tokens[0].char_pos, Globals, Tokens[0])
                    if not ok:
                        ThrowTypeError("READ prompt must be a string", Tokens[0], Globals)

                try:
                    res = input(prompt if prompt is not None else "")
                except Exception as e:
                    ThrowRuntimeError(f"Error reading input: {str(e)}", Tokens[0], Globals)

                return res

    if Tokens and Tokens[0].type == "data_type":
        p_idx = None
        for idx, t in enumerate(Tokens[1:], start=1):
            if t.type == "left_parenthesis":
                p_idx = idx
                break
        if p_idx is not None:
            depth = 0
            q_idx = None
            for j in range(p_idx, len(Tokens)):
                tt = Tokens[j]
                if tt.type == "left_parenthesis":
                    depth += 1
                elif tt.type == "right_parenthesis":
                    depth -= 1
                    if depth == 0:
                        q_idx = j
                        break
            if q_idx is not None:
                dtype = Tokens[0].aux
                inner = Tokens[p_idx+1:q_idx]
                val = EvalExpr(inner, Globals, Lines) if inner else None
                converted, ok = Convert(val, dtype, True, Tokens[0].line, Tokens[0].char_pos, Globals, Tokens[0])
                if ok:
                    return converted
                else:
                    ThrowRuntimeError("Invalid expression", Tokens[0], Globals)

    def get_value(token: Token, idx: int = None, tokens: list[Token] = None):
        if token.type in ["numeric_literal", "string_literal", "list_literal", "dict_literal"]:
            return token.value

        if token.type == "identifier":
            if token.aux and not isinstance(token.aux, str):
                return token.aux
            name = token.value
            if name in Globals["VARIABLES"]:
                return Globals["VARIABLES"][name].value
            else:
                ThrowNameError(f"Variable '{name}' not defined inside the current scope", token, Globals)

        return token.value

    if len(Tokens) == 1:
        return get_value(Tokens[0])

    operators = {
        '+': 1, '-': 1,
        '*': 2, '/': 2,
        '^': 3,
        '<': 0, '>': 0, '==': 0, '!=': 0, '<=': 0, '>=': 0
    }

    output = []
    operator_stack = []

    for idx, token in enumerate(Tokens):
        if token.type == "whitespace":
            continue

        if token.type in ["numeric_literal", "string_literal", "list_literal", "dict_literal", "identifier"]:
            val = None
            try:
                val = get_value(token, idx, Tokens)
            except Exception:
                output.append(token)
                continue

            if isinstance(val, str):
                output.append(Token("string_literal", val, token.line, token.char_pos))
            elif isinstance(val, (int, float)):
                output.append(Token("numeric_literal", val, token.line, token.char_pos))
            elif isinstance(val, list):
                output.append(Token("list_literal", val, token.line, token.char_pos))
            elif isinstance(val, dict):
                output.append(Token("dict_literal", val, token.line, token.char_pos))
            elif val is None:
                output.append(Token("empty", None, token.line, token.char_pos))
            else:
                output.append(Token("identifier", token.value, token.line, token.char_pos, 0, val))

        elif hasattr(token, "value") and token.value in operators:
            while (
                operator_stack
                and hasattr(operator_stack[-1], "value")
                and operator_stack[-1].value in operators
                and (
                    (token.value != "^" and operators[operator_stack[-1].value] >= operators[token.value])
                    or (token.value == "^" and operators[operator_stack[-1].value] > operators[token.value])
                )
            ):
                output.append(operator_stack.pop())
            operator_stack.append(token)

        elif token.type in ("left_paren", "left_parenthesis", "open_paren"):
            operator_stack.append(token)

        elif token.type in ("right_paren", "right_parenthesis", "close_paren"):
            while operator_stack and operator_stack[-1].type not in ("left_paren", "left_parenthesis", "open_paren"):
                output.append(operator_stack.pop())
            if operator_stack and operator_stack[-1].type in ("left_paren", "left_parenthesis", "open_paren"):
                operator_stack.pop()

        else:
            output.append(token)

    while operator_stack:
        output.append(operator_stack.pop())

    stack = []
    for token in output:
        if token.type in ["numeric_literal", "string_literal", "list_literal", "dict_literal", "identifier", "empty"]:
            try:
                stack.append(get_value(token))
            except OverflowError:
                ThrowValueError("Numeric value too large", token, Globals)
            except Exception as e:
                ThrowRuntimeError(str(e), token, Globals)
        else:
            if len(stack) < 2:
                if len(stack) == 1:
                    return stack[0]
                else:
                    ThrowRuntimeError("Invalid expression", Tokens[0], Globals)

            b = stack.pop()
            a = stack.pop()

            if isinstance(a, str) or isinstance(b, str):
                if getattr(token, "value", None) == '+':
                    try:
                        stack.append(str(a) + str(b))
                    except Exception as e:
                        ThrowRuntimeError(f"Error during string concatenation: {str(e)}", token, Globals)
                else:
                    ThrowTypeError("Invalid operation for strings", token, Globals)
                continue

            try:
                op = getattr(token, "value", None)
                if op == '+':
                    stack.append(a + b)
                elif op == '-':
                    stack.append(a - b)
                elif op == '*':
                    stack.append(a * b)
                elif op == '/':
                    if b == 0:
                        ThrowRuntimeError("Division by zero", token, Globals)
                    stack.append(a / b)
                elif op == '^':
                    stack.append(a ** b)
                elif op == '<':
                    stack.append(1 if a < b else 0)
                elif op == '>':
                    stack.append(1 if a > b else 0)
                elif op == '<=':
                    stack.append(1 if a <= b else 0)
                elif op == '>=':
                    stack.append(1 if a >= b else 0)
                elif op == '==':
                    stack.append(1 if a == b else 0)
                elif op == '!=':
                    stack.append(1 if a != b else 0)
                else:
                    ThrowRuntimeError(f"Unknown operator '{op}'", token, Globals)
            except OverflowError:
                ThrowValueError("Numeric value too large", token, Globals)
            except TypeError:
                ThrowTypeError("Invalid operation between types", token, Globals)

    if len(stack) != 1:
        if all(isinstance(x, (int, float, str)) for x in stack):
            return stack
        else:
            ThrowRuntimeError("Invalid expression", Tokens[0], Globals)

    return stack[0]

def ClearWhitespace(Tokens: list[Token]) -> list[Token]:
    from copy import copy
    tmpTokens:list[Token] = []

    for t in Tokens:
        if t.type != "whitespace":
            tmpTokens.append(t)

    return tmpTokens

def SetVariable(Globals: dict, Name: str, Value: any, IsConst: bool, Type: str, Line: int, CharPos: int, token: Token = Token("whitespace", " ", 1, 1, 0, "")):
    if Name in Globals["VARIABLES"]:
        Type = Globals["VARIABLES"][Name].type

        if Globals["VARIABLES"][Name].const:
            ThrowRuntimeError(f"Cannot reassign constant variable: {Name}", token, Globals)

    Value, _ = Convert(Value, Type, True, Line, CharPos, Globals, token)
    if Value is None:
        typeval = "VOID"
    elif isinstance(Value, bool):
        typeval = "BOOL"
    elif isinstance(Value, int):
        typeval = "INT"
    elif isinstance(Value, float):
        typeval = "FLOAT"
    elif isinstance(Value, str):
        typeval = "STR"
    elif isinstance(Value, list):
        typeval = "LIST"
    elif isinstance(Value, dict):
        typeval = "DICT"
    else:
        typeval = type(Value).__name__.upper() if type(Value).__name__.upper() != "NONETYPE" else "VOID"

    if (Type or "").upper() == "*":
        inferred_type = typeval
        Globals["VARIABLES"][Name] = Variable(Name, Value, IsConst, inferred_type)
        return

    if typeval == Type.upper():
        Globals["VARIABLES"][Name] = Variable(Name, Value, IsConst, Type)
    elif typeval == "STR" and Type == "CHAR":
        if len(Value) > 1:
            ThrowTypeError("Cannot assign string to char type variable", token, Globals)
        Globals["VARIABLES"][Name] = Variable(Name, Value[0], IsConst, Type)
    elif typeval == Type == "VOID":
        if Value != None:
            ThrowTypeError("Cannot assign value to Void type variable", token, Globals)
        Globals["VARIABLES"][Name] = Variable(Name, None, IsConst, Type)

    elif isinstance(Value, object) and hasattr(Value, "class_def") and Type.startswith("INSTANCE_OF_"):
        Globals["VARIABLES"][Name] = Variable(Name, Value, IsConst, Type)
        return

    else:
        ThrowTypeError(f"Cannot assign type {type(Value).__name__.upper()} to variable type {Type.upper()}", token, Globals)

def GetVariable(Globals: dict, Name: str, Token: Token = Token("whitespace", " ", 1, 1, 0, "")) -> list | str | int | float | dict:
    if Name in Globals["VARIABLES"]:
        return Globals["VARIABLES"][Name]

    ThrowNameError(f"Variable '{Name}' not defined inside the current scope", Token, Globals)

def Validate(keyword:str, line:int=1, char_pos:int=1, Globals:dict={}, Lines:list[str]=[], d:int=0) -> Token:
    global DEPTH
    char_pos-=1
    keyword = keyword.strip()
    if keyword in ['\\n', '\\t', '\\r', '\\0', '\n', '\t', '\r', '\0']:
        return Token("rawbyte", codecs.encode(keyword, "raw_unicode_escape"), line, char_pos)

    if keyword in keywords:
        return Token("keyword", keyword, line, char_pos, DEPTH, keyword)
    elif keyword.isdecimal() or keyword.isnumeric() or re.match(r'^\d+\.\d+$', keyword):
        return Token("numeric_literal", int(keyword) if '.' not in keyword else float(keyword), line, char_pos, DEPTH, keyword)
    elif (keyword.startswith("'") and keyword.endswith("'")) or (keyword.startswith('"') and keyword.endswith('"')):
        return Token("string_literal", keyword[1:-1], line, char_pos, DEPTH, keyword)
    elif keyword == ';':
        return Token("semicolon", ';', line, char_pos, DEPTH, keyword)
    elif keyword in ['+', '-', '*', '/', '%', '^']:
        return Token("arithmetic_operator", keyword, line, char_pos, DEPTH, keyword)
    elif keyword in ['<', '>', '<=', '>=', '==', '!=']:
        return Token("relational_operator", keyword, line, char_pos, DEPTH, keyword)
    elif keyword in ['&&', '||', '!']:
        return Token("logical_operator", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == '.':
        return Token("dot", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == '(':
        DEPTH+=1
        return Token("left_parenthesis", keyword, line, char_pos, DEPTH-1, keyword)
    elif keyword == ')':
        DEPTH-=1
        if DEPTH < 0:
            ThrowSyntaxError("Unclosed right parenthesis", Token("right_parenthesis", keyword, line, char_pos), Globals)
        return Token("right_parenthesis", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == ',':
        return Token("comma", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == ':':
        return Token("colon", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == '[':
        DEPTH+=1
        return Token("left_bracket", keyword, line, char_pos, DEPTH-1, keyword)
    elif keyword == ']':
        DEPTH-=1
        return Token("right_bracket", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == '=':
        return Token("assignment_operator", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == 'CONST':
        return Token("const", keyword, line, char_pos, DEPTH, keyword)
    elif keyword in datatypes:
        return Token("data_type", keyword, line, char_pos, DEPTH, keyword)
    elif keyword in ["NULL", "NONE", "VOID"]:
        return Token("empty", keyword, line, char_pos, DEPTH, keyword)
    elif keyword in ["TRUE", "FALSE"]:
        return Token("boolean_literal", keyword, line, char_pos, DEPTH, keyword)
    elif keyword == "{":
        DEPTH += 1
        return Token("left_key", keyword, line, char_pos, DEPTH-1, keyword)
    elif keyword == "}":
        DEPTH -= 1
        return Token("right_key", keyword, line, char_pos, DEPTH, keyword)
    


    elif keyword.isspace() or keyword == '':
        return Token("whitespace", " ", line, char_pos, DEPTH, keyword)
    else:
        return Token("identifier", keyword, line, char_pos, DEPTH, keyword)
    
def Convert(value: any, target_type: str, error: bool, line: int, char_pos: int, globals: dict = {}, token: Token = Token("whitespace", " ", 1, 1, 0, "")):
    target_type = (target_type or "").upper()

    def is_quoted_string(s):
        if not isinstance(s, str):
            return False, s
        s = s.strip()
        if (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
            return True, s[1:-1]
        return False, s

    if target_type.upper() in ("NULL", "NONE", "VOID"):
        if value is None:
            return None, True
        if error:
            ThrowTypeError(f"Cannot assign non-None value to VOID type", token, globals)
        return value, False

    if target_type == "STR":
        if isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit:
                return unq, True
            else:
                if globals and value in globals.get("VARIABLES", {}):
                    var_val = globals["VARIABLES"][value].value
                    if isinstance(var_val, str):
                        return var_val, True
                return value, True
        elif isinstance(value, (int, float, dict, list)):
            return str(value), True
        else:
            return str(value).upper(), True

    if target_type == "INT":
        if isinstance(value, int):
            return value, True
        elif isinstance(value, float):
            if value.is_integer():
                return int(value), True
            else:
                if error:
                    ThrowTypeError(f"Cannot convert FLOAT with decimal part to INT", token, globals)
                return value, False
        elif isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit and unq.isnumeric():
                try:
                    return int(value), True
                except ValueError:
                    if error:
                        ThrowTypeError(f"Tried to convert non-numeric STRING literal to INT", token, globals)
                    return value, False
            elif is_str_lit:
                if error:
                    ThrowTypeError(f"Cannot convert STRING literal to INT", token, globals)
                return value, False
            else:
                # try to parse plain string input (e.g., from READ())
                try:
                    return int(unq), True
                except Exception:
                    # fallback: treat as variable name lookup
                    if globals and unq in globals.get("VARIABLES", {}):
                        var_val = globals["VARIABLES"][unq].value
                        if isinstance(var_val, (int, float, str)):
                            try:
                                if var_val == True: var_val = 1
                                elif var_val == False: var_val = 0
                                return int(var_val), True
                            except ValueError:
                                if isinstance(var_val, dict):
                                    ThrowTypeError(f"Cannot convert DICT to INT", token, globals)
                                elif isinstance(var_val, list):
                                    ThrowTypeError(f"Cannot convert LIST to INT", token, globals)
                                else:
                                    ThrowTypeError(f"Cannot convert NULL to INT")
                if error:
                    ThrowTypeError(f"Cannot convert STRING to INT", token, globals)
                return value, False
        else:
            if error and isinstance(value, dict):
                ThrowTypeError(f"Cannot convert DICT to INT", token, globals)
            elif error and isinstance(value, list):
                ThrowTypeError(f"Cannot convert LIST to INT", token, globals)
            elif error:
                ThrowTypeError(f"Cannot convert NULL to INT", token, globals)
            return value, False

    if target_type == "FLOAT":
        if isinstance(value, float):
            return value, True
        elif isinstance(value, int):
            return float(value), True
        elif isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit:
                if error:
                    ThrowTypeError(f"Cannot convert string literal to FLOAT", token, globals)
                return value, False
            else:
                if globals and unq in globals.get("VARIABLES", {}):
                    var_val = globals["VARIABLES"][unq].value
                    if isinstance(var_val, int, float, str):
                        try:
                            if var_val == True: var_val = 1
                            elif var_val == False: var_val = 0
                            return float(var_val), True
                        except ValueError:
                            if isinstance(var_val, dict):
                                ThrowTypeError(f"Cannot convert DICT to FLOAT", token, globals)
                            elif isinstance(var_val, list):
                                ThrowTypeError(f"Cannot convert LIST to FLOAT", token, globals)
                            else:
                                ThrowTypeError(f"Cannot convert NULL to FLOAT")
                if error:
                    ThrowTypeError(f"Cannot convert STRING to FLOAT", token, globals)
                return value, False
        else:
            if error and isinstance(value, dict):
                ThrowTypeError(f"Cannot convert DICT to FLOAT", token, globals)
            elif error and isinstance(value, list):
                ThrowTypeError(f"Cannot convert LIST to FLOAT", token, globals)
            elif error:
                ThrowTypeError(f"Cannot convert NULL to FLOAT", token, globals)
            return value, False

    if target_type == "BOOL":
        if isinstance(value, bool):
            return value, True
        elif isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit:
                if error:
                    ThrowTypeError(f"Cannot convert STRING literal to BOOL", token, globals)
                return value, False
            else:
                if unq in ("TRUE", "FALSE"):
                    return unq == "TRUE", True
                if error:
                    ThrowTypeError(f"Cannot convert STRING to BOOL", token, globals)
                return value, False
        elif isinstance(value, int):
            if error:
                ThrowTypeError(f"Cannot convert INT to BOOL", token, globals)
        elif isinstance(value, float):
            if error:
                ThrowTypeError(f"Cannot convert FLOAT to BOOL", token, globals)
            return value, False
        else:
            if error and isinstance(value, dict):
                ThrowTypeError(f"Cannot convert DICT to BOOL", token, globals)
            elif error and isinstance(value, list):
                ThrowTypeError(f"Cannot convert LIST to BOOL", token, globals)
            elif error:
                ThrowTypeError(f"Cannot convert NULL to BOOL", token, globals)
            return value, False

    if target_type == "CHAR":
        if isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            s = unq if is_str_lit else value
            if len(s) == 1:
                return s, True
            else:
                if error:
                    ThrowTypeError(f"CHAR must be exactly one character, got {len(s)}", token, globals)
                return value, False
        elif isinstance(value, bool):
            return 'T' if value else 'F', True
        else:
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to CHAR", token, globals)
            return value, False

    if target_type == "LIST":
        if isinstance(value, list):
            return value, True
        elif isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit and unq.startswith("[") and unq.endswith("]"):
                return eval(unq), True
            else:
                if error:
                    ThrowTypeError(f"Cannot convert STRING to LIST", token, globals)
                return value, False
        else:
            if error and isinstance(value, dict):
                ThrowTypeError(f"Cannot convert DICT to LIST", token, globals)
            elif error and isinstance(value, int):
                ThrowTypeError(f"Cannot convert INT to LIST", token, globals)
            if error and isinstance(value, bool):
                ThrowTypeError(f"Cannot convert BOOL to DICT", token, globals)
            elif error:
                ThrowTypeError(f"Cannot convert NULL to LIST", token, globals)
            return value, False

    if target_type == "DICT":
        if isinstance(value, dict):
            return value, True
        elif isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit and unq.startswith("{") and unq.endswith("}"):
                return eval(unq), True
            else:
                if error:
                    ThrowTypeError(f"Cannot convert STRING to DICT", token, globals)
                return value, False
        else:
            if error and isinstance(value, float):
                ThrowTypeError(f"Cannot convert FLOAT to DICT", token, globals)
            if error and isinstance(value, list):
                ThrowTypeError(f"Cannot convert LIST to DICT", token, globals)
            if error and isinstance(value, bool):
                ThrowTypeError(f"Cannot convert BOOL to DICT", token, globals)
            elif error and isinstance(value, int):
                ThrowTypeError(f"Cannot convert INT to BOOL", token, globals)
            elif error:
                ThrowTypeError(f"Cannot convert NULL to BOOL", token, globals)
            return value, False

    if isinstance(value, object) and hasattr(value, "class_def") and target_type.startswith("INSTANCE_OF_"):
        return value, True

    if target_type == "*":
        for data in datatypes:
            v, s = Convert(value, data, error, 0, 0, globals, token)
            if s:
                return v, s
        return value, False

    if error:
        ThrowTypeError(f"Unsupported target type: {target_type}", token, globals)
    return value, False

def IsString(s):
    if not isinstance(s, str):
        return False
    s = s.strip()
    if (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
        return True
    return False

def PrintDict(callingline: int, d: dict, indent: int = 0, skiplbl: list = []):
    print(str(callingline)+"\n" if callingline != -1 else "", end="")
    for key, value in d.items():
        if str(key) not in skiplbl:
            print(' ' * indent + str(key) + ':', end=' ')
            if isinstance(value, dict):
                PrintDict(-1, value, indent + 4)
            elif isinstance(value, list):
                print('[')
                for item in value:
                    if isinstance(item, dict):
                        PrintDict(-1, item, indent + 4)
                    else:
                        print(' ' * (indent + 4) + str(item))
                print(' ' * indent + ']')
            else:
                print(str(value))

    print()

def PrintTable(data):
    str_data = [[str(x) for x in row] for row in data]
    col_widths = [
        max(len(row[i]) for row in str_data)
        for i in range(len(str_data[0]))
    ]

    fmt = (' '*4+"| ").join("{:<" + str(w) + "}" for w in col_widths)
    first = True
    for row in str_data:
        print(fmt.format(*row))

def TriggerBreakpoint(Token: Token, Globals: dict):
    dataVars = [
        ["Name", "Value", "Is Const", "Type"],
        *[[Globals["VARIABLES"][v].name, Globals["VARIABLES"][v].value, Globals["VARIABLES"][v].const, Globals["VARIABLES"][v].type] for v in Globals["VARIABLES"].keys()]
    ]

    dataFunc = [
        ["Name", "Parameters", "Static", "Type"],
        *[[Globals["FUNCTIONS"][v].name, Globals["FUNCTIONS"][v].parameters, Globals["FUNCTIONS"][v].static, Globals["FUNCTIONS"][v].type] for v in Globals["FUNCTIONS"].keys()]
    ]

    dataClass = [
        ["Name", "Properties", "Parent", "Instances"],
        *[[Globals["CLASSES"][v].name, Globals["CLASSES"][v].properties, Globals["CLASSES"][v].parent_class, Globals["CLASSES"][v].instances] for v in Globals["CLASSES"].keys()]
    ]

    dataLabel = [
        ["Name", "Location"],
        *[[Globals["LABELS"][v].name, Globals["LABELS"][v].pos] for v in Globals["LABELS"].keys()]
    ]

    print("\n")

    if len(Globals["VARIABLES"]) != 0:
        print("Variables")
        PrintTable(dataVars)

        print('')
    
    if len(Globals["FUNCTIONS"]) != 0:
        print("Functions")
        PrintTable(dataFunc)
        
        print('')
    
    if len(Globals["CLASSES"]) != 0:
        print("Classes")
        PrintTable(dataClass)
        
        print('')
    
    if len(Globals["LABELS"]) != 0:
        print("Labels")
        PrintTable(dataLabel)
        
        print('')

    print("\nBreakpoint called from line:", Token.line, "char:", Token.char_pos)

    os.system("pause")

def ParseSingleIdentifier(tokens, Globals):
    for t in tokens:
        if t.type == "identifier":
            return t
    if tokens:
        ThrowSyntaxError("Expected identifier before ':' in FOR expression", tokens[0], Globals)
    else:
        ThrowSyntaxError("Expected identifier before ':' in FOR expression", Token("whitespace", " ", 1, 1), Globals)

def ParseTwoIdentifiers(tokens, Globals):
    ids = [t for t in tokens if t.type == "identifier"]

    if len(ids) != 2:
        if tokens:
            ThrowSyntaxError("FOR dictionary loop must use: FOR (key, value : DICT)", tokens[0], Globals)
        else:
            ThrowSyntaxError("FOR dictionary loop must use: FOR (key, value : DICT)", Token("whitespace", " ", 1, 1), Globals)

    return ids[0], ids[1]

def ExecuteVariableDeclaration(tokens, Globals, Lines):
    i=0
    if not tokens:
        return

    if tokens[0].type != "data_type":
        ThrowSyntaxError("Variable declaration in FOR must start with a datatype", tokens[0], Globals)

    var_type = tokens[0].aux

    i+=1
    i = ClearWhitespace(tokens, i)
    if i >= len(tokens):
        ThrowSyntaxError("Expected variable name in FOR declaration", tokens[i-1] if len(tokens)>1 else tokens[0], Globals)


    next_token = tokens[i]
    if next_token.type != "identifier":
        ThrowSyntaxError("Expected variable name in FOR declaration", next_token, Globals)

    i+=1
    i = ClearWhitespace(tokens, i)
    if i >= len(tokens):
        ThrowSyntaxError("Expected assignment operator in FOR declaration", next_token, Globals)

    next_token2 = tokens[i]

    if next_token2.type != "assignment_operator":
        ThrowSyntaxError("Expected assignment operator in FOR declaration", next_token2, Globals)

    i+=1
    i = ClearWhitespace(tokens, i)
    if i >= len(tokens):
        ThrowSyntaxError("Expected value after assignment operator in FOR declaration", next_token2, Globals)
    
    expr = tokens[i:]
    value = EvalExpr(expr, Globals, Lines)

    SetVariable(Globals, next_token.aux, value, False, var_type, tokens[0].line, tokens[0].char_pos, tokens[0])

def RunFor3Parts(expr_tokens, body_tokens, Globals, Lines):
    parts = [[]]
    for t in expr_tokens:
        if t.type == "semicolon":
            parts.append([])
        else:
            parts[-1].append(t)

    if len(parts) != 3:
        ThrowSyntaxError("Classic FOR must be: FOR(init; cond; step)", 
                         parts[0][0] if parts[0] else Token("whitespace", " ", 1, 1),
                         Globals)

    init, cond, step = parts

    if init:
        ExecuteVariableDeclaration(init, Globals, Lines)
        loop_var_name = None
        for t in init:
            if t.type == "identifier":
                loop_var_name = t.aux
                break

    while True:
        if cond:
            c = EvalExpr(cond, Globals, Lines)
            if not c:
                break
        
        from MowTypes import Token
        from copy import deepcopy
        import sys
        Run = None
        if "__main__" in sys.modules and hasattr(sys.modules["__main__"], "Run"):
            Run = getattr(sys.modules["__main__"], "Run")
        else:
            from mowlang import Run

        local_body = deepcopy(body_tokens)

        local_body.append(Token("EOF", None, 1, 1))
        try:
            Run(local_body)
        except BreakException:
            break
        except ContinueException:
            pass

        if step:
            eq_idx = next((idx for idx, t in enumerate(step) if t.type == "assignment_operator"), None)
            if eq_idx is not None:
                lhs = [t for t in step[:eq_idx] if t.type != "whitespace"]
                rhs = [t for t in step[eq_idx+1:] if t.type != "whitespace"]
                if lhs and lhs[0].type == "identifier":
                    val = EvalExpr(rhs, Globals, Lines)
                    SetVariable(Globals, lhs[0].aux, val, False, "*", lhs[0].line, lhs[0].char_pos, lhs[0])
                else:
                    try:
                        EvalExpr(step, Globals, Lines)
                    except:
                        pass
            else:
                try:
                    EvalExpr(step, Globals, Lines)
                except:
                    pass
        

def RunForIterable(expr_tokens, body_tokens, Globals, Lines):
    left = []
    right = []
    found = False
    for t in expr_tokens:
        if t.type == "colon" and not found:
            found = True
            continue
        if not found:
            left.append(t)
        else:
            right.append(t)

    if not left or not right:
        ThrowSyntaxError("FOREACH must be: FOR (item : iterable)", left[0] if left else Token("whitespace", " ", 1, 1), Globals)

    iterable = EvalExpr(right, Globals, Lines)

    if any(t.type == "comma" for t in left):
        key_tok, val_tok = ParseTwoIdentifiers(left, Globals)

        if not isinstance(iterable, dict):
            ThrowTypeError("Expected DICT in FOR (k, v : dict)", key_tok, Globals)

        for k, v in iterable.items():
            SetVariable(Globals, key_tok.aux, k, False, "*", key_tok.line, key_tok.char_pos, key_tok)
            SetVariable(Globals, val_tok.aux, v, False, "*", val_tok.line, val_tok.char_pos, val_tok)

            from copy import deepcopy
            local_body = deepcopy(body_tokens)
            local_body.append(Token("EOF", None, 1, 1))

            import sys
            Run = None
            if "__main__" in sys.modules and hasattr(sys.modules["__main__"], "Run"):
                Run = getattr(sys.modules["__main__"], "Run")
            else:
                from mowlang import Run
            try:
                Run(local_body)
            except BreakException:
                break
            except ContinueException:
                continue
        return

    item_tok = ParseSingleIdentifier(left, Globals)

    if not isinstance(iterable, (list, tuple, str)):
        ThrowTypeError("Iterable in FOR(item : iterable) must be LIST, TUPLE or STRING", item_tok, Globals)

    for x in iterable:
        SetVariable(Globals, item_tok.aux, x, False, "*", item_tok.line, item_tok.char_pos, item_tok)

        from copy import deepcopy
        local_body = deepcopy(body_tokens)
        local_body.append(Token("EOF", None, 1, 1))

        import sys
        Run = None
        if "__main__" in sys.modules and hasattr(sys.modules["__main__"], "Run"):
            Run = getattr(sys.modules["__main__"], "Run")
        else:
            from mowlang import Run
        try:
            Run(local_body)
        except BreakException:
            break
        except ContinueException:
            continue

class BreakException(Exception):
    pass

class ContinueException(Exception):
    pass

def RunWhile(condition_tokens, body_tokens, Globals, Lines):
    from MowTypes import Token
    from copy import deepcopy
    import sys
    Run = None
    if "__main__" in sys.modules and hasattr(sys.modules["__main__"], "Run"):
        Run = getattr(sys.modules["__main__"], "Run")
    else:
        from mowlang import Run
    
    while True:
        cond = EvalExpr(condition_tokens, Globals, Globals["LINES"])
        if not cond:
            break
        
        local_body = deepcopy(body_tokens)
        local_body.append(Token("EOF", None, 1, 1))
        
        try:
            Run(local_body)
        except BreakException:
            break
        except ContinueException:
            continue
