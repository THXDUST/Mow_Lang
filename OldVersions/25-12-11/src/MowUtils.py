import codecs, re
from MowErrors import ThrowNameError, ThrowRuntimeError, ThrowNotImplementedError, ThrowSyntaxError, ThrowTypeError, ThrowValueError
from MowTypes import Token, Variable

datatypes = ["INT", "FLOAT", "STR", "BOOL", "LIST", "DICT", "CHAR", "VOID"]

keywords = ["PRINT", "PRINTL", "EXIT", "LABEL", "FUNCTION", "GOTO", "RETURN", "CLASS", "FOR"]
#              0         1        2       3          4         5        6        7       8

DEPTH = 0

def EvalExpr(Tokens: list[Token], Globals: dict, Lines: list[str]) -> any:
    if len(Tokens) == 0:
        return None
    # DEBUG: mostrar tokens recebidos (remova/ajuste após diagnóstico)
    # print("[DEBUG EvalExpr] Tokens:", [(t.type, getattr(t,'value',None), getattr(t,'aux',None)) for t in Tokens])

    # --- Resolver acessos com ponto (ex: this.name) ---
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
                    # criar token com tipo adequado (não "evaluated")
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
                        # instância ou objeto qualquer -> tratar como identifier contendo objeto
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

    # --- Lista literal [ ... ] handling (mantive seu comportamento original) ---
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

    def get_value(token: Token, idx: int = None, tokens: list[Token] = None):
        # literais simples
        if token.type in ["numeric_literal", "string_literal", "list_literal", "dict_literal"]:
            return token.value

        # se token foi criado com identifier mas com objeto no .aux (caso de instância)
        if token.type == "identifier":
            if token.aux and not isinstance(token.aux, str):
                return token.aux
            name = token.value
            if name in Globals["VARIABLES"]:
                return Globals["VARIABLES"][name].value
            else:
                ThrowNameError(f"Variable '{name}' not defined inside the current scope", token, Globals)

        # fallback
        return token.value

    if len(Tokens) == 1:
        return get_value(Tokens[0])

    # operadores e precedências
    operators = {
        '+': 1, '-': 1,
        '*': 2, '/': 2,
        '^': 3
    }

    output = []
    operator_stack = []

    for idx, token in enumerate(Tokens):
        # Ignorar espaços em branco já tokenizados
        if token.type == "whitespace":
            continue
        # Se for literal ou identifier: tentar resolver e inserir com tipo correto
        if token.type in ["numeric_literal", "string_literal", "list_literal", "dict_literal", "identifier"]:
            val = None
            try:
                val = get_value(token, idx, Tokens)
            except Exception:
                # se não der pra resolver agora, apenas copie o token original
                output.append(token)
                continue

            # se val for já um objeto/instância, preservamos como identifier com .aux tendo o objeto
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
                # qualquer objeto (instância) vira identifier com aux=object
                output.append(Token("identifier", token.value, token.line, token.char_pos, 0, val))

        # operador aritmético (assumimos que token.value contém o símbolo)
        elif hasattr(token, "value") and token.value in operators:
            # associatividade especial para ^
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
            # tokens inesperados: copie-os (permitir símbolos como ',' etc., será tratado acima)
            output.append(token)

    # despejar operadores restantes
    while operator_stack:
        output.append(operator_stack.pop())

    # --- Avaliação do postfix ---
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
            # operador
            if len(stack) < 2:
                if len(stack) == 1:
                    return stack[0]
                else:
                    # debug print do postfix para diagnóstico
                    # print("-" + "\n-".join([str(t) for t in output]))
                    ThrowRuntimeError("Invalid expression", Tokens[0], Globals)

            b = stack.pop()
            a = stack.pop()

            # concatenation / string handling
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

def ClearWhitespace(Tokens: list[Token], i: int) -> int:
    if i < 0:
        i = 0

    if i < len(Tokens) and Tokens[i].type == "whitespace":
        i+=1

    while i < len(Tokens) and Tokens[i].type == "whitespace":
        i+=1

    return i

def SetVariable(Globals: dict, Name: str, Value: any, IsConst: bool, Type: str, Line: int, CharPos: int, token: Token = Token("whitespace", " ", 1, 1, 0, "")):
    if Name in Globals["VARIABLES"]:
        Type = Globals["VARIABLES"][Name].type

        if Globals["VARIABLES"][Name].const:
            ThrowRuntimeError(f"Cannot reassign constant variable: {Name}", token, Globals)

    Value, _ = Convert(Value, Type, True, Line, CharPos, Globals, token)

    if isinstance(Value, (int, float)):
        try:
            Value = int(Value)
        except ValueError:
            Value = float(Value)

    typeval = type(Value).__name__.upper() if type(Value).__name__.upper() != "NONETYPE" else "VOID"

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

    # --- Suporte para instâncias de classes ---
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

    def parse_number_literal(s):
        try:
            n = float(s)
            if n.is_integer():
                return int(n), True, "int"
            return n, True, "float"
        except Exception:
            return s, False, None

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
        elif isinstance(value, (int, float, bool)):
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to STR", token, globals)
            return value, False
        else:
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to STR", token, globals)
            return value, False

    if target_type == "INT":
        if isinstance(value, int):
            return value, True
        elif isinstance(value, float):
            if value.is_integer():
                return int(value), True
            else:
                if error:
                    ThrowTypeError(f"Cannot convert float with decimal part to INT", token, globals)
                return value, False
        elif isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit:
                if error:
                    ThrowTypeError(f"Cannot convert string literal to INT", token, globals)
                return value, False
            else:
                if globals and unq in globals.get("VARIABLES", {}):
                    var_val = globals["VARIABLES"][unq].value
                    if isinstance(var_val, int):
                        return var_val, True
                if error:
                    ThrowTypeError(f"Cannot convert string to INT", token, globals)
                return value, False
        else:
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to INT", token, globals)
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
                    if isinstance(var_val, (int, float)):
                        return float(var_val), True
                if error:
                    ThrowTypeError(f"Cannot convert string to FLOAT", token, globals)
                return value, False
        else:
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to FLOAT", token, globals)
            return value, False

    if target_type == "BOOL":
        if isinstance(value, bool):
            return value, True
        elif isinstance(value, str):
            is_str_lit, unq = is_quoted_string(value)
            if is_str_lit:
                if error:
                    ThrowTypeError(f"Cannot convert string literal to BOOL", token, globals)
                return value, False
            else:
                if unq.lower() in ("true", "false"):
                    return unq.lower() == "true", True
                if error:
                    ThrowTypeError(f"Cannot convert string to BOOL", token, globals)
                return value, False
        elif isinstance(value, (int, float)):
            if error:
                ThrowTypeError(f"Cannot convert number to BOOL", token, globals)
            return value, False
        else:
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to BOOL", token, globals)
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
                    ThrowTypeError(f"Cannot convert string to LIST", token, globals)
                return value, False
        else:
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to LIST", token, globals)
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
                    ThrowTypeError(f"Cannot convert string to DICT", token, globals)
                return value, False
        else:
            if error:
                ThrowTypeError(f"Cannot convert {type(value).__name__.upper() if type(value).__name__.upper() != 'NONETYPE' else 'NULL'} to DICT", token, globals)
            return value, False

    if isinstance(value, object) and hasattr(value, "class_def") and target_type.startswith("INSTANCE_OF_"):
        return value, True

    if target_type == "*":
        return value, True

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







