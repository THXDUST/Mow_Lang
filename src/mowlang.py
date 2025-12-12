import MowImports as MI

MI.init(autoreset=True)

class MowLangInfo:
    NAME: MI.Final[str] = "MowLang"
    AUTHOR: MI.Final[str] = "WIIKDEV"
    RELEASE_DATE: MI.Final[str] = "2024-07-04"
    VERSION = "0.0.1"
    BUILD = 1
    RECENT_UPDATE = "2024-07-04"

    def Update(self) -> None:
        try:
            with open(MI.os.path.dirname(MI.os.path.abspath(__file__))+"/VERSION") as f:
                data = f.readlines()
                self.VERSION = data[0].strip()
                self.BUILD = int(data[1].strip())
                self.RECENT_UPDATE = data[2].strip()
        except FileNotFoundError:
            pass

        return None

LanguageInfo: MowLangInfo = MowLangInfo()
LanguageInfo.Update()

keywords = MI.keywords

DEBUG = False

GLOBALS = {
    "FILE": "",
    "LINES": [],
    "LABELS": {},
    "VARIABLES": {},
    "FUNCTIONS": {},
    "CLASSES": {},
    "FILE_PATHS": []
}

program_filepath = ""
if len(MI.sys.argv) > 1: program_filepath = MI.sys.argv[1]
else: program_filepath = False
is_debug = len(MI.sys.argv) > 2 and MI.sys.argv[2].lower() == "--debug"

for arg in MI.sys.argv:
    if arg.endswith(".mow"):
        GLOBALS["FILE_PATHS"].append(arg)
    
    elif arg == "--debug":
        DEBUG = True

TOKENS: list[MI.Token] = []
CURRENT_TOKEN: MI.Token = None
DEPTH = 0

DATA_TYPES = ["INT", "FLOAT", "STR", "BOOL", "LIST", "DICT", "CHAR", "VOID"]
VAR_TYPE = "VOID"
MAKE_CONST = False
LINES = []

class ReturnException(Exception):
    def __init__(self, value):
        super().__init__("Function return")
        self.value = value

def OpenFile(filePath:str) -> str:
    GLOBALS["FILE"] = MI.os.path.abspath(filePath)
    temp = open(filePath, 'r').read()
    return temp

def Tokenize(FILE:str):
    global DEPTH
    tmpTokens: list[MI.Token] = []
    line = 1
    char_pos = 1
    i = 0
    buffer = ""
    in_string = False
    string_quote = None
    escape = False

    should_ignore = False
    block_comment = False

    while i < len(FILE):
        ch = FILE[i]

        if ch == "\n":
            line += 1
            char_pos = 1
        else:
            char_pos += 1

        if ch == '/' and i+1 < len(FILE) and FILE[i+1] == '*' and not block_comment:
            block_comment = True
            should_ignore = True
            i += 2
            continue
        elif ch == '*' and i+1 < len(FILE) and FILE[i+1] == '/' and block_comment:
            block_comment = False
            should_ignore = False
            i += 2
            continue

        if in_string:
            buffer += ch
            if escape:
                escape = False
            elif ch == '\\':
                escape = True
            elif ch == string_quote:
                tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                buffer = ""
                in_string = False
                string_quote = None
        elif should_ignore or block_comment:
            if ch == '\n':
                should_ignore = False
        elif ch == '/' and i+1 < len(FILE) and FILE[i+1] == '/':
            should_ignore = True
        else:
            if ch in ('"', "'"):
                if buffer:
                    tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                    buffer = ""
                in_string = True
                string_quote = ch
                buffer += ch
            elif ch in ['{', '[', '(']:
                if buffer:
                    tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                    buffer = ""

                tok = MI.Validate(ch, line, char_pos, GLOBALS, GLOBALS["LINES"], DEPTH)
                tmpTokens.append(tok)
                DEPTH+=1
            elif ch in ['}', ']', ")"]:
                if buffer:
                    tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                    buffer = ""
                DEPTH -= 1
                tok = MI.Validate(ch, line, char_pos, GLOBALS, GLOBALS["LINES"], DEPTH)
                tmpTokens.append(tok)
            elif ch.isspace():
                if buffer:
                    tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                    buffer = ""
                tmpTokens.append(MI.Validate(ch, line, char_pos, GLOBALS, GLOBALS["LINES"], DEPTH))
            elif ch.isalnum():
                buffer += ch
            elif ch == '.':
                try:
                    float(buffer)
                    buffer+=ch
                except ValueError:
                    tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                    buffer = ""
                    tmpTokens.append(MI.Validate(ch, line, char_pos, GLOBALS, GLOBALS["LINES"], DEPTH))
            elif not buffer and ch == '\\':
                buffer += ch
            else:
                if buffer:
                    tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                    buffer = ""
                
                next_ch = FILE[i+1] if i+1 < len(FILE) else ''
                two = ch + next_ch
                if two in ['<=', '>=', '==', '!=', '&&', '||']:
                    tmpTokens.append(MI.Validate(two, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
                    i += 1
                else:
                    tmpTokens.append(MI.Validate(ch, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))

        if len(tmpTokens) > 0 and tmpTokens[-1].type == "rawbyte":
            MI.ThrowSyntaxError("Maybe missing ';'?", tmpTokens[-1], GLOBALS)

        i += 1

    if buffer:
        tmpTokens.append(MI.Validate(buffer, line, char_pos - len(buffer), GLOBALS, GLOBALS["LINES"], DEPTH))
    
    tmpTokens.append(MI.Token("EOF", None, line, char_pos))

    return tmpTokens

def Run(Tokens: list[MI.Token] = []):
    global MAKE_CONST, VAR_TYPE, CURRENT_TOKEN, DEPTH
    i = -1

    while i < len(Tokens):
        i = MI.ClearWhitespace(Tokens, i)
        if i >= len(Tokens):
            break

        token = Tokens[i]
        CURRENT_TOKEN = token
        i+=1

        i = MI.ClearWhitespace(Tokens, i)
        if i >= len(Tokens):
            break
        
        if token.type == "EOF" and DEPTH == 0:
            break

        elif token.type == "EOF" and DEPTH > 0:
            MI.ThrowSyntaxError("Unmatched left parenthesis", Tokens[i-2], GLOBALS)

        elif token.type == "semicolon" and DEPTH == 0:
            continue

        elif token.type == "semicolon" and DEPTH > 0:
            MI.ThrowSyntaxError("Unlosed left parenthesis", token, GLOBALS)

        elif token.type == "keyword":
            if token.aux in (keywords[0], keywords[1]):  # PRINT
                nxt_line = token.aux[-1] == 'L'

                next_token = Tokens[i]
                i += 1
                if next_token.type == "left_parenthesis":
                    expr_tokens = []
                    while i < len(Tokens):
                        token2 = Tokens[i]
                        i+=1
                        if token2.type == "right_parenthesis" and token2.depth == next_token.depth:
                            break

                        expr_tokens.append(token2)

                    result = str(MI.EvalExpr(expr_tokens, GLOBALS, GLOBALS["LINES"])) if expr_tokens else ""
                    print(MI.codecs.decode(result, 'unicode_escape'), end='\n' if nxt_line else '')
                    try:
                        i = MI.ClearWhitespace(Tokens, i)
                        semicolon_token = Tokens[i]
                        if semicolon_token.type != "semicolon":
                            MI.ThrowSyntaxError("Expected ';' after PRINT statement", token2, GLOBALS)
                    except IndexError:
                        MI.ThrowSyntaxError("Expected ';' after PRINT statement", token2, GLOBALS)
                else:
                    MI.ThrowSyntaxError("Expected '(' after PRINT", token, GLOBALS)
                continue

            elif token.aux == keywords[2]:  # EXIT
                next_token = Tokens[i]
                i+=1

                if next_token.type == "left_parenthesis":
                    try:
                        i = MI.ClearWhitespace(Tokens, i)
                        if i >= len(Tokens):
                            break

                        next_token2 = Tokens[i]
                        i+=1
                        if next_token2.type == "numeric_literal":
                            expr = [next_token2]
                            depth = 0
                            while i < len(Tokens):
                                i = MI.ClearWhitespace(Tokens, i)
                                if i >= len(Tokens):
                                    break

                                next_token3 = Tokens[i]
                                i+=1
                                if next_token3.type == "left_parenthesis":
                                    depth += 1
                                    expr.append(next_token3)
                                elif next_token3.type == "right_parenthesis":
                                    if depth == 0:
                                        exit_code = MI.EvalExpr(expr, GLOBALS, LINES)
                                        i-=1
                                        break
                                    depth -= 1
                                    expr.append(next_token3)
                                else:
                                    expr.append(next_token3)

                        elif next_token2.type == "right_parenthesis":
                            exit_code = 0
                            MI.MowLangQuit("Program exited succesfully.", exit_code, GLOBALS, next_token2.line, next_token2.char_pos)
                        elif next_token2.type == "identifier":
                            try:
                                exit_code = int(GLOBALS["VARIABLES"][next_token2.aux].value)
                            except KeyError:
                                MI.ThrowNameError(f"Undefined variable inside current scope: {next_token2.aux}", next_token2, GLOBALS)
                            except ValueError:
                                MI.ThrowTypeError(f"Cannot convert variable '{next_token2.aux}' to integer for EXIT code", next_token2, GLOBALS)
                        else:
                            MI.ThrowTypeError("Expected numeric literal or integer variable for EXIT code", next_token2, GLOBALS)

                        try:
                            i = MI.ClearWhitespace(Tokens, i)
                            if i >= len(Tokens):
                                break

                            next_token3 = Tokens[i]
                            if next_token3.type == "right_parenthesis":
                                MI.MowLangQuit("Program exited succesfully.", exit_code, GLOBALS, next_token3.line, next_token3.char_pos)
                            else:
                                MI.ThrowSyntaxError("Expected ')' after EXIT code", next_token3, GLOBALS)
                        except Exception as e:
                            MI.ThrowSyntaxError("Expected ')' after EXIT code", next_token2, GLOBALS)


                    except IndexError:
                        MI.ThrowTypeError("Expected numeric literal, variable or left parenthesis for EXIT code", next_token, GLOBALS)
                    continue

                else:
                    MI.ThrowSyntaxError("Expected '(' after EXIT", next_token, GLOBALS)
                    continue

            elif token.aux == keywords[3]: # LABEL
                next_token: MI.Token = Tokens[i]
                i+=1

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break

                if next_token.type == "left_parenthesis":
                    expr: list[MI.Token] = []
                    while i < len(Tokens):
                        i = MI.ClearWhitespace(Tokens, i)
                        if i >= len(Tokens):
                            break

                        next_token2: MI.Token = Tokens[i]
                        i+=1

                        if next_token2.type == "right_parenthesis" and next_token2.depth == next_token.depth:
                            if len(expr) == 0:
                                MI.ThrowTypeError("LABEL takes 1 argument 0 recieved", next_token2, GLOBALS)
                            elif len(expr) == 1:
                                i_s = MI.IsString(expr[0].aux)
                                if not i_s:
                                    result = MI.EvalExpr(expr, GLOBALS, GLOBALS["LINES"])
                                else:
                                    result = expr[0].value
                            else:
                                result = MI.EvalExpr(expr, GLOBALS, GLOBALS["LINES"])
                            
                            tr = type(result)
                            if tr in [bool, int, float, dict, list]:
                                MI.ThrowTypeError(f"Cannot use {'BOOL' if tr == bool else ('INT' if tr == float else ('FLOAT' if tr == float else ('DICT' if tr == dict else 'LIST')))} to name LABELS", next_token, GLOBALS)

                            i = MI.ClearWhitespace(Tokens, i)
                            if i >= len(Tokens):
                                break

                            next_token3 = Tokens[i]
                            i+=1

                            if next_token3.type != "semicolon":
                                MI.ThrowSyntaxError("Expected semicolon after ')'", next_token2, GLOBALS)
                            
                            GLOBALS["LABELS"][result] = MI.Label(result, i)
                            break
                        
                        expr.append(next_token2)

                else:
                    MI.ThrowSyntaxError("Expected '(' after LABEL statement", next_token, GLOBALS)

            elif token.aux == keywords[4]:  # FUNCTION
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break

                ret_type = "VOID"
                is_const = False
                if Tokens[i].type == "data_type":
                    ret_type = Tokens[i].aux
                    i += 1
                    i = MI.ClearWhitespace(Tokens, i)

                if i < len(Tokens) and Tokens[i].type == "const":
                    is_const = True
                    i += 1
                    i = MI.ClearWhitespace(Tokens, i)

                if i >= len(Tokens):
                    break
                name_tok: MI.Token = Tokens[i]
                i += 1
                if name_tok.type != "identifier":
                    MI.ThrowSyntaxError("Expected function name after FUNCTION (and optional type/CONST)", name_tok, GLOBALS)

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens) or Tokens[i].type != "left_parenthesis":
                    MI.ThrowSyntaxError("Expected '(' after function name", Tokens[i] if i < len(Tokens) else name_tok, GLOBALS)
                i += 1

                params: list[MI.FunctionParam] = []
                param_buf: list[MI.Token] = []
                depth = 0
                def flush_param(buf: list[MI.Token]):
                    if not buf:
                        return
                    
                    toklist = [t for t in buf if t.type != "whitespace"]
                    if not toklist:
                        return
                    
                    if toklist[0].type != "identifier":
                        MI.ThrowSyntaxError("Parameter must start with identifier", toklist[0], GLOBALS)
                    pname = toklist[0].aux
                    ptype = "*"
                    pdefault = MI.NotNull()
                    
                    colon_idx = next((idx for idx,t in enumerate(toklist) if t.type == "colon"), None)
                    eq_idx = next((idx for idx,t in enumerate(toklist) if t.type == "assignment_operator"), None)
                    if colon_idx is not None:
                        if colon_idx+1 >= len(toklist) or toklist[colon_idx+1].type != "data_type":
                            MI.ThrowSyntaxError("Expected type after ':' in parameter", toklist[colon_idx], GLOBALS)
                        ptype = toklist[colon_idx+1].aux
                    if eq_idx is not None:
                        def_tokens = toklist[eq_idx+1:]
                        if not def_tokens:
                            MI.ThrowSyntaxError("Expected default value after '='", toklist[eq_idx], GLOBALS)
                            
                        try:
                            val = MI.EvalExpr(def_tokens, GLOBALS, GLOBALS["LINES"])
                        except Exception as e:
                            MI.ThrowRuntimeError(str(e), def_tokens[0], GLOBALS)
                            
                        if ptype != "*":
                            val, ok = MI.Convert(val, ptype, True, name_tok.line, name_tok.char_pos, GLOBALS, def_tokens[0])
                        pdefault = val
                    params.append(MI.FunctionParam(pname, ptype, pdefault))

                while i < len(Tokens):
                    t = Tokens[i]
                    i += 1
                    if t.type == "left_parenthesis":
                        depth += 1
                        param_buf.append(t)
                    elif t.type == "right_parenthesis":
                        if depth == 0:
                            flush_param(param_buf)
                            param_buf = []
                            break
                        depth -= 1
                        param_buf.append(t)
                    elif t.type == "comma" and depth == 0:
                        flush_param(param_buf)
                        param_buf = []
                    else:
                        if t.type != "whitespace":
                            param_buf.append(t)

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break
                body_start = Tokens[i]
                i += 1
                if body_start.type != "left_key":
                    MI.ThrowSyntaxError("Expected '{' to start function body", body_start, GLOBALS)

                body_tokens: list[MI.Token] = []
                while i < len(Tokens):
                    t2 = Tokens[i]
                    i += 1
                    if t2.type == "right_key" and t2.depth == body_start.depth:
                        break
                    body_tokens.append(t2)

                GLOBALS["FUNCTIONS"][name_tok.aux] = MI.Function(name_tok.aux, params, body_tokens, ret_type, is_const)
                continue

            elif token.aux == keywords[5]: # GOTO
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break

                next_token = Tokens[i]
                i+=1

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break

                if next_token.type != "left_parenthesis":
                    MI.ThrowSyntaxError("Expected '(' after GOTO statement", next_token, GLOBALS)

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break


                expr: list[MI.Token] = []
                while i < len(Tokens):
                    next_token2 = Tokens[i]

                    i+=1
                    i = MI.ClearWhitespace(Tokens, i)
                    if i >= len(Tokens):
                        break

                    if next_token2.type == "right_parenthesis" and next_token2.depth == next_token.depth:
                        if not expr:
                            MI.ThrowSyntaxError("Label identifier must not me empty", next_token2, GLOBALS)
                        break

                    expr.append(next_token2)

                result = MI.EvalExpr(expr, GLOBALS, GLOBALS["LINES"])
                result, s = MI.Convert(result, "STR", True, next_token2.line, next_token2.char_pos, GLOBALS, next_token2)
                i2 = i

                try:
                    i2 = GLOBALS["LABELS"][result].pos
                except KeyError:
                    MI.ThrowRuntimeError(f"Label named '{result}' is not defined inside current scope", next_token2, GLOBALS)
                except Exception as e:
                    MI.ThrowRuntimeError(str(e), next_token2, GLOBALS)
                finally:
                    next_token3 = Tokens[i]

                    i+=1
                    i = MI.ClearWhitespace(Tokens, i)
                    if i >= len(Tokens):
                        break

                    if next_token3.type != "semicolon":
                        MI.ThrowSyntaxError("Expected ';' after GOTO statement", next_token2, GLOBALS)

                    i = i2
                    continue
            
            elif token.aux == keywords[6]:  # RETURN
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break

                next_token = Tokens[i]
                i += 1

                if next_token.type != "left_parenthesis":
                    MI.ThrowSyntaxError("Expected '(' after RETURN", next_token, GLOBALS)

                expr_tokens: list[MI.Token] = []
                while i < len(Tokens):
                    t = Tokens[i]
                    i += 1
                    if t.type == "right_parenthesis" and t.depth == next_token.depth:
                        break
                    expr_tokens.append(t)

                if expr_tokens:
                    result = MI.EvalExpr(expr_tokens, GLOBALS, GLOBALS["LINES"])
                else:
                    result = None

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected ';' after RETURN", Tokens[i-1] if i-1 < len(Tokens) else next_token, GLOBALS)
                sem_tok = Tokens[i]
                i += 1
                if sem_tok.type != "semicolon":
                    MI.ThrowSyntaxError("Expected ';' after RETURN", sem_tok, GLOBALS)

                if not GLOBALS.get("_in_function", False):
                    MI.ThrowRuntimeError("RETURN can only be used inside a function", token, GLOBALS)

                raise ReturnException(result)

            elif token.aux == keywords[7]: # CLASS
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected class name", token, GLOBALS)

                class_name_token = Tokens[i]
                i += 1
                
                if class_name_token.type != "identifier":
                    MI.ThrowSyntaxError(f"Class name must be an identifier, not {class_name_token.type.upper()}", class_name_token, GLOBALS)
                
                class_name = class_name_token.aux
                
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected '{' after class name", class_name_token, GLOBALS)

                if Tokens[i].type != "left_key":  # '{'
                    MI.ThrowSyntaxError(f"Expected '{{', got '{Tokens[i].value}' ({Tokens[i].type})", Tokens[i], GLOBALS)
                
                body_start_token = Tokens[i]
                i += 1

                class_body_tokens = []
                depth = 1
                
                while i < len(Tokens):
                    current_token = Tokens[i]
                    i += 1
                    
                    if current_token.type == "right_key":  # '}'
                        depth -= 1
                        if depth == 0:
                            break
                        class_body_tokens.append(current_token)
                    elif current_token.type == "left_key":  # '{'
                        depth += 1
                        class_body_tokens.append(current_token)
                    else:
                        class_body_tokens.append(current_token)
                
                if depth != 0:
                    MI.ThrowSyntaxError("Unmatched '{' in class definition", body_start_token, GLOBALS)

                methods = {}
                properties = {}
                
                j = 0
                while j < len(class_body_tokens):
                    j = MI.ClearWhitespace(class_body_tokens, j)
                    if j >= len(class_body_tokens):
                        break
                        
                    member_token = class_body_tokens[j]
                    j += 1
                    
                    if member_token.type == "keyword" and member_token.aux == "FUNCTION":
                        j = MI.ClearWhitespace(class_body_tokens, j)
                        if j >= len(class_body_tokens):
                            break
                            
                        ret_type = "VOID"
                        is_const = False
                        
                        if j < len(class_body_tokens) and class_body_tokens[j].type == "data_type":
                            ret_type = class_body_tokens[j].aux
                            j += 1
                            j = MI.ClearWhitespace(class_body_tokens, j)
                        
                        if j >= len(class_body_tokens) or class_body_tokens[j].type != "identifier":
                            MI.ThrowSyntaxError("Expected method name", 
                                            class_body_tokens[j-1] if j > 0 else member_token, GLOBALS)
                        
                        method_name_token = class_body_tokens[j]
                        method_name = method_name_token.aux
                        j += 1
                        
                        if j >= len(class_body_tokens) or class_body_tokens[j].type != "left_parenthesis":
                            MI.ThrowSyntaxError("Expected '(' after method name", method_name_token, GLOBALS)
                        
                        param_start = class_body_tokens[j]
                        j += 1
                        
                        params = []
                        param_buf = []
                        param_depth = 0
                        
                        def flush_param(buf):
                            if not buf:
                                return
                            
                            toklist = [t for t in buf if t.type != "whitespace"]
                            if not toklist:
                                return
                            
                            if toklist[0].type != "identifier":
                                MI.ThrowSyntaxError("Parameter must start with identifier", toklist[0], GLOBALS)
                            
                            pname = toklist[0].aux
                            ptype = "*"
                            pdefault = MI.NotNull()
                            
                            colon_idx = next((idx for idx,t in enumerate(toklist) if t.type == "colon"), None)
                            eq_idx = next((idx for idx,t in enumerate(toklist) if t.type == "assignment_operator"), None)
                            
                            if colon_idx is not None:
                                if colon_idx+1 >= len(toklist) or toklist[colon_idx+1].type != "data_type":
                                    MI.ThrowSyntaxError("Expected type after ':' in parameter", toklist[colon_idx], GLOBALS)
                                ptype = toklist[colon_idx+1].aux
                            
                            if eq_idx is not None:
                                def_tokens = toklist[eq_idx+1:]
                                if not def_tokens:
                                    MI.ThrowSyntaxError("Expected default value after '='", toklist[eq_idx], GLOBALS)
                                
                                try:
                                    val = MI.EvalExpr(def_tokens, GLOBALS, GLOBALS["LINES"])
                                except Exception as e:
                                    MI.ThrowRuntimeError(str(e), def_tokens[0], GLOBALS)
                                
                                if ptype != "*":
                                    val, ok = MI.Convert(val, ptype, True, method_name_token.line, method_name_token.char_pos, GLOBALS, def_tokens[0])
                                pdefault = val
                            
                            params.append(MI.FunctionParam(pname, ptype, pdefault))
                        
                        while j < len(class_body_tokens):
                            t = class_body_tokens[j]
                            j += 1
                            
                            if t.type == "left_parenthesis":
                                param_depth += 1
                                param_buf.append(t)
                            elif t.type == "right_parenthesis":
                                if param_depth == 0:
                                    flush_param(param_buf)
                                    param_buf = []
                                    break
                                param_depth -= 1
                                param_buf.append(t)
                            elif t.type == "comma" and param_depth == 0:
                                flush_param(param_buf)
                                param_buf = []
                            else:
                                if t.type != "whitespace":
                                    param_buf.append(t)
                        
                        j = MI.ClearWhitespace(class_body_tokens, j)
                        if j >= len(class_body_tokens):
                            MI.ThrowSyntaxError("Expected '{' to start method body", 
                                            class_body_tokens[j-1] if j > 0 else param_start, GLOBALS)
                        
                        body_start = class_body_tokens[j]
                        j += 1
                        
                        if body_start.type != "left_key":
                            MI.ThrowSyntaxError("Expected '{' to start method body", body_start, GLOBALS)
                        
                        method_body_tokens = []
                        method_depth = 1
                        
                        while j < len(class_body_tokens):
                            t2 = class_body_tokens[j]
                            j += 1
                            
                            if t2.type == "right_key":
                                method_depth -= 1
                                if method_depth == 0:
                                    break
                                method_body_tokens.append(t2)
                            elif t2.type == "left_key":
                                method_depth += 1
                                method_body_tokens.append(t2)
                            else:
                                method_body_tokens.append(t2)
                        
                        if method_depth != 0:
                            MI.ThrowSyntaxError("Unmatched '{' in method definition", body_start, GLOBALS)
                        
                        method_func = MI.Function(method_name, params, method_body_tokens, ret_type, is_const)
                        methods[method_name] = method_func
                        
                    elif member_token.type == "data_type":
                        prop_type = member_token.aux
                        
                        j = MI.ClearWhitespace(class_body_tokens, j)
                        if j >= len(class_body_tokens):
                            MI.ThrowSyntaxError("Expected property name", member_token, GLOBALS)
                        
                        prop_name_token = class_body_tokens[j]
                        j += 1
                        
                        if prop_name_token.type != "identifier":
                            MI.ThrowSyntaxError("Property name must be an identifier", prop_name_token, GLOBALS)
                        
                        prop_name = prop_name_token.aux
                        is_const = False
                        
                        j = MI.ClearWhitespace(class_body_tokens, j)
                        if j < len(class_body_tokens) and class_body_tokens[j].type == "const":
                            is_const = True
                            j += 1
                            j = MI.ClearWhitespace(class_body_tokens, j)
                        
                        if j < len(class_body_tokens) and class_body_tokens[j].type == "assignment_operator":
                            j += 1
                            while j < len(class_body_tokens) and class_body_tokens[j].type != "semicolon":
                                j += 1
                            if j < len(class_body_tokens):
                                j += 1
                        
                        elif j < len(class_body_tokens) and class_body_tokens[j].type == "semicolon":
                            j += 1
                        else:
                            MI.ThrowSyntaxError("Expected ';' after property declaration", 
                                            class_body_tokens[j-1] if j > 0 else prop_name_token, GLOBALS)
                        
                        properties[prop_name] = (prop_type, is_const)
                    
                    elif member_token.type == "semicolon":
                        continue
                    else:
                        #MI.ThrowSyntaxError(f"Unexpected token in class body: {member_token.type}", member_token, GLOBALS)
                        j = MI.ClearWhitespace(class_body_tokens, j)
                        
                new_class = MI.Class(class_name, methods, properties, None)
                GLOBALS["CLASSES"][class_name] = new_class
                
                continue

            elif token.aux == keywords[8]:
                # FOR (once; condition; always) --> FOR (INT i=0; i < 100; i++)
                # FOR (item : list iterable) --> FOR (const item : LIST([1, 2, 3, "apple", "pie"]))
                # FOR (key, item : dict iterable) --> FOR (const k, const v : DICT({"Key": "Value"}))

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected '(' after FOR", token, GLOBALS)

                next_token = Tokens[i]
                i += 1
                i = MI.ClearWhitespace(Tokens, i)
                if next_token.type != "left_parenthesis":
                    MI.ThrowSyntaxError("Expected '(' after FOR", next_token, GLOBALS)

                expr_tokens = []
                while i < len(Tokens):
                    t = Tokens[i]
                    i += 1
                    if t.type == "right_parenthesis" and t.depth == next_token.depth:
                        break
                    expr_tokens.append(t)

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected '{' after FOR(...)", token, GLOBALS)

                body_start = Tokens[i]
                i += 1
                i = MI.ClearWhitespace(Tokens, i)
                if body_start.type != "left_key":
                    MI.ThrowSyntaxError("Expected '{' after FOR(...)", body_start, GLOBALS)

                body_tokens = []
                depth = 1
                while i < len(Tokens):
                    t = Tokens[i]
                    i += 1
                    if t.type == "left_key":
                        depth += 1
                        body_tokens.append(t)
                        continue
                    if t.type == "right_key":
                        depth -= 1
                        if depth == 0:
                            break
                        body_tokens.append(t)
                        continue
                    body_tokens.append(t)

                has_semicolon = any(t.type == "semicolon" for t in expr_tokens)
                has_colon = any(t.type == "colon" for t in expr_tokens)

                if has_semicolon and not has_colon:
                    MI.RunFor3Parts(expr_tokens, body_tokens, GLOBALS, GLOBALS["LINES"])
                elif has_colon and not has_semicolon:
                    MI.RunForIterable(expr_tokens, body_tokens, GLOBALS, GLOBALS["LINES"])
                else:
                    MI.ThrowSyntaxError("Ambiguous or invalid FOR syntax", token, GLOBALS)
                continue

            elif token.aux == keywords[9]:  # WHILE
                # WHILE (condition) { body }
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected '(' after WHILE", token, GLOBALS)

                next_token = Tokens[i]
                i += 1
                i = MI.ClearWhitespace(Tokens, i)
                if next_token.type != "left_parenthesis":
                    MI.ThrowSyntaxError("Expected '(' after WHILE", next_token, GLOBALS)

                condition_tokens = []
                while i < len(Tokens):
                    t = Tokens[i]
                    i += 1
                    if t.type == "right_parenthesis" and t.depth == next_token.depth:
                        break
                    condition_tokens.append(t)

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected '{' after WHILE(...)", token, GLOBALS)

                body_start = Tokens[i]
                i += 1
                i = MI.ClearWhitespace(Tokens, i)
                if body_start.type != "left_key":
                    MI.ThrowSyntaxError("Expected '{' after WHILE(...)", body_start, GLOBALS)

                body_tokens = []
                depth = 1
                while i < len(Tokens):
                    t = Tokens[i]
                    i += 1
                    if t.type == "left_key":
                        depth += 1
                        body_tokens.append(t)
                        continue
                    if t.type == "right_key":
                        depth -= 1
                        if depth == 0:
                            break
                        body_tokens.append(t)
                        continue
                    body_tokens.append(t)

                MI.RunWhile(condition_tokens, body_tokens, GLOBALS, GLOBALS["LINES"])
                continue

            elif token.aux == keywords[10]:  # BREAK
                raise MI.BreakException()

            elif token.aux == keywords[11]:  # CONTINUE
                raise MI.ContinueException()

            elif token.aux == keywords[12]: # BREAKPOINT
                if DEBUG:
                    i = MI.ClearWhitespace(Tokens, i)
                    if i >= len(Tokens):
                        MI.ThrowSyntaxError("Expected parenthesis '('", token, GLOBALS)
                    
                    next_token = Tokens[i]
                    i+=1
                    i = MI.ClearWhitespace(Tokens, i)
                    if next_token.type != 'left_parenthesis':
                        MI.ThrowSyntaxError("Expected parenthesis '('", next_token, GLOBALS)
                    
                    next_token = Tokens[i]
                    i+=1
                    i = MI.ClearWhitespace(Tokens, i)
                    if next_token.type != 'right_parenthesis':
                        MI.ThrowSyntaxError("BREAKPOINT takes no parameters but 1 was provided", next_token, GLOBALS)

                    next_token = Tokens[i]
                    i+=1
                    i = MI.ClearWhitespace(Tokens, i)
                    if next_token.type != 'semicolon':
                        MI.ThrowSyntaxError("Expected semicolon ';'", next_token, GLOBALS)

                    MI.TriggerBreakpoint(token, GLOBALS)
                continue

        elif token.type == "const":
            MAKE_CONST = True
            continue
        
        elif token.type == "data_type":
            VAR_TYPE = token.aux
            continue

        elif token.type == "identifier":
            i = MI.ClearWhitespace(Tokens, i)
            if i >= len(Tokens):
                break

            next_token = Tokens[i]
            
            if next_token.type == "assignment_operator":
                i += 1
                expr: list[MI.Token] = []
                val: any = None
                
                while i < len(Tokens):
                    i = MI.ClearWhitespace(Tokens, i)
                    if i >= len(Tokens):
                        break

                    next_token2 = Tokens[i]
                    
                    if next_token2.type == "semicolon":
                        is_function_call = False
                        func_name_token = None

                        for idx, tok in enumerate(expr):
                            if (tok.type == "identifier" and 
                                tok.aux in GLOBALS["FUNCTIONS"]):
                                for j in range(idx + 1, len(expr)):
                                    next_tok = expr[j]
                                    if next_tok.type == "whitespace":
                                        continue
                                    elif next_tok.type == "left_parenthesis":
                                        is_function_call = True
                                        func_name_token = tok
                                        break
                                    else:
                                        break
                                if is_function_call:
                                    break

                        if is_function_call and func_name_token:
                            func = GLOBALS["FUNCTIONS"][func_name_token.aux]
                            
                            args_exprs = []
                            current_expr = []
                            depth = 0
                            in_args = False
                            found_opening_paren = False

                            for idx, tok in enumerate(expr):
                                if tok.type == "left_parenthesis" and not found_opening_paren:
                                    found_opening_paren = True
                                    in_args = True
                                    continue
                                
                                if not found_opening_paren:
                                    continue
                                    
                                if tok.type == "right_parenthesis" and depth == 0 and in_args:
                                    if current_expr:
                                        args_exprs.append(current_expr)
                                    break
                                elif tok.type == "comma" and depth == 0 and in_args:
                                    if current_expr:
                                        args_exprs.append(current_expr)
                                        current_expr = []
                                else:
                                    if tok.type == "left_parenthesis":
                                        depth += 1
                                    elif tok.type == "right_parenthesis":
                                        depth -= 1
                                    
                                    if in_args and tok.type != "whitespace":
                                        current_expr.append(tok)

                            args_values = []

                            for ae in args_exprs:
                                if not ae:
                                    args_values.append(None)
                                else:
                                    args_values.append(MI.EvalExpr(ae, GLOBALS, GLOBALS["LINES"]))

                            old_vars = GLOBALS["VARIABLES"].copy()
                            old_globals_vars = GLOBALS["VARIABLES"]
                            GLOBALS["VARIABLES"] = {}
                            old_in_func = GLOBALS.get("_in_function", False)
                            retval = None

                            try:
                                for idx, p in enumerate(func.parameters):
                                    pname = p.name
                                    ptype = p.type or "*"
                                    
                                    if idx < len(args_values) and args_values[idx] is not None:
                                        aval = args_values[idx]
                                    else:
                                        if hasattr(p, "default") and not isinstance(p.default, type(MI.NotNull())):
                                            aval = p.default
                                        else:
                                            MI.ThrowRuntimeError(f"Missing argument for parameter '{pname}'", func_name_token, GLOBALS)

                                    MI.SetVariable(GLOBALS, pname, aval, False, ptype, func_name_token.line, func_name_token.char_pos, func_name_token)

                                body_tokens = func.body.copy()
                                body_tokens.append(MI.Token("EOF", None, func_name_token.line, func_name_token.char_pos))
                                
                                current_var_type = VAR_TYPE
                                current_make_const = MAKE_CONST
                                old_depth = DEPTH
                                
                                GLOBALS["_in_function"] = True
                                try:
                                    VAR_TYPE = "VOID"
                                    MAKE_CONST = False
                                    Run(body_tokens)
                                except ReturnException as rexc:
                                    retval = rexc.value
                                except KeyboardInterrupt:
                                    MI.ThrowKeyboardInterruptError("'CONTROL' + 'C' keys pressed, interrupting program...", CURRENT_TOKEN, GLOBALS)
                                finally:
                                    tret = type(retval).__name__.upper() if type(retval).__name__.upper() != "NONETYPE" else "VOID"
                                    tretf = func.type.upper()
                                    if tret != tretf:
                                        MI.ThrowTypeError(f"Cannot return {type(retval).__name__.upper()} through {func.type.upper()} function", CURRENT_TOKEN, GLOBALS)

                                    VAR_TYPE = current_var_type
                                    MAKE_CONST = current_make_const
                                    DEPTH = old_depth
                            finally:
                                GLOBALS["VARIABLES"] = old_globals_vars
                                GLOBALS["_in_function"] = old_in_func

                            if retval is not None:
                                converted_val, success = MI.Convert(retval, VAR_TYPE, True, token.line, token.char_pos, GLOBALS, func_name_token)
                                if success:
                                    val = converted_val
                                else:
                                    MI.ThrowTypeError(f"Cannot convert function return value to {VAR_TYPE}", func_name_token, GLOBALS)
                            else:
                                val = None
                                
                            MI.SetVariable(GLOBALS, token.aux, val, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, func_name_token)
                        else:
                            try:
                                #print("> "+"\n> ".join([str(t) for t in expr]))
                                val = MI.EvalExpr(expr, GLOBALS, GLOBALS["LINES"])
                            except Exception as e:
                                val, s = MI.Convert("".join([str(t.aux) for t in expr]), VAR_TYPE, False, token.line, token.char_pos, GLOBALS, expr[0] if expr else token)
                                if not s:
                                    MI.ThrowRuntimeError(f"Error evaluating expression: {str(e)}", expr[0] if expr else token, GLOBALS)
                            
                            MI.SetVariable(GLOBALS, token.aux, val, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, expr[0] if expr else token)
                        
                        break

                    expr.append(next_token2)
                    i += 1
                
                VAR_TYPE = "VOID"
                MAKE_CONST = False
                continue
            
            i += 1

            if next_token.type == "left_parenthesis":
                func_name = token.aux
                func = GLOBALS["FUNCTIONS"].get(func_name)
                if func is None:
                    MI.ThrowNameError(f"Function '{func_name}' not defined", token, GLOBALS)

                args_exprs = []
                expr = []
                depth = 0
                while i < len(Tokens):
                    t = Tokens[i]
                    i += 1
                    if t.type == "right_parenthesis" and t.depth == next_token.depth:
                        if expr:
                            args_exprs.append(expr)
                        break
                    elif t.type == "comma" and t.depth == next_token.depth:
                        args_exprs.append(expr)
                        expr = []
                    elif t.type != "whitespace":
                        expr.append(t)

                args_values = []
                for ae in args_exprs:
                    if not ae:
                        args_values.append(None)
                    else:
                        args_values.append(MI.EvalExpr(ae, GLOBALS, GLOBALS["LINES"]))

                old_globals_vars = MI.deepcopy(GLOBALS["VARIABLES"])
                GLOBALS["VARIABLES"] = {}
                old_in_func = GLOBALS.get("_in_function", False)
                retval = None

                try:
                    for idx, p in enumerate(func.parameters):
                        pname = p.name
                        ptype = p.type or "*"
                        
                        if idx < len(args_values) and args_values[idx] is not None:
                            aval = args_values[idx]
                        else:
                            if hasattr(p, "default") and not isinstance(p.default, type(MI.NotNull())):
                                aval = p.default
                            else:
                                MI.ThrowRuntimeError(f"Missing argument for parameter '{pname}'", token, GLOBALS)

                        MI.SetVariable(GLOBALS, pname, aval, False, ptype, token.line, token.char_pos, token)

                    body_tokens = func.body.copy()
                    body_tokens.append(MI.Token("EOF", None, token.line, token.char_pos))
                    
                    current_var_type = VAR_TYPE
                    current_make_const = MAKE_CONST
                    old_depth = DEPTH
                    
                    GLOBALS["_in_function"] = True
                    try:
                        VAR_TYPE = "VOID"
                        MAKE_CONST = False
                        
                        Run(body_tokens)
                    except ReturnException as rexc:
                        retval = rexc.value
                    finally:
                        VAR_TYPE = current_var_type
                        MAKE_CONST = current_make_const
                        DEPTH = old_depth
                finally:
                    GLOBALS["VARIABLES"] = old_globals_vars
                    GLOBALS["_in_function"] = old_in_func

                if VAR_TYPE != "VOID":
                    if retval is not None:
                        converted_val, success = MI.Convert(retval, VAR_TYPE, True, token.line, token.char_pos, GLOBALS, token)
                        if success:
                            MI.SetVariable(GLOBALS, token.aux, converted_val, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, token)
                        else:
                            MI.ThrowTypeError(f"Cannot convert function return value to {VAR_TYPE}", token, GLOBALS)
                    else:
                        MI.SetVariable(GLOBALS, token.aux, None, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, token)
                else:
                    retval = None

                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected ';' after function call", token, GLOBALS)
                sem_tok = Tokens[i]
                i += 1
                if sem_tok.type != "semicolon":
                    MI.ThrowSyntaxError("Expected ';' after function call", sem_tok, GLOBALS)

                VAR_TYPE = "VOID"
                MAKE_CONST = False
                continue

            elif token.aux in GLOBALS["CLASSES"]:
                class_name = token.aux
                class_def = GLOBALS["CLASSES"][class_name]
                
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected identifier for instance name", token, GLOBALS)
                
                instance_name_token = next_token
                
                if instance_name_token.type != "identifier":
                    MI.ThrowSyntaxError("Instance name must be an identifier", instance_name_token, GLOBALS)
                
                instance_name = instance_name_token.aux
                
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    MI.ThrowSyntaxError("Expected ';' after instance declaration", instance_name_token, GLOBALS)
                
                if Tokens[i].type != "semicolon":
                    MI.ThrowSyntaxError("Expected ';' after instance declaration", Tokens[i], GLOBALS)
                
                semicolon_token = Tokens[i]
                i += 1
                
                instance = MI.Instance(class_def, instance_name)
                GLOBALS["VARIABLES"][instance_name] = MI.Variable(instance_name, instance, False, f"INSTANCE_OF_{class_name}")
                
                continue

            elif token.type == "identifier" and token.aux in GLOBALS["VARIABLES"]:
                var = GLOBALS["VARIABLES"][token.aux]
                #MI.PrintDict(935, GLOBALS, 0, ["LINES", "FILE"])
                
                i = MI.ClearWhitespace(Tokens, i)
                if i >= len(Tokens):
                    break

                next_token = Tokens[i]
                i += 1
                
                if next_token.type == "dot":
                    i = MI.ClearWhitespace(Tokens, i)
                    if i >= len(Tokens):
                        MI.ThrowSyntaxError("Expected member name after '.'", next_token, GLOBALS)
                    
                    member_token = Tokens[i]
                    i += 1
                    
                    if member_token.type != "identifier":
                        MI.ThrowSyntaxError("Member name must be an identifier", member_token, GLOBALS)
                    
                    member_name = member_token.aux
                    
                    i = MI.ClearWhitespace(Tokens, i)
                    if i >= len(Tokens):
                        MI.ThrowSyntaxError("Expected '(' for method call or ';' for property access", 
                                        member_token, GLOBALS)
                    
                    next_member_token = Tokens[i]
                    
                    if next_member_token.type == "left_parenthesis":
                        if hasattr(var.value, 'class_def') and member_name in var.value.class_def.methods:
                            method = var.value.class_def.methods[member_name]
                            
                            call_start = Tokens[i]
                            i += 1
                            
                            args_exprs = []
                            expr = []
                            depth = 0
                            
                            while i < len(Tokens):
                                t = Tokens[i]
                                i += 1
                                
                                if t.type == "right_parenthesis" and t.depth == call_start.depth:
                                    if expr:
                                        args_exprs.append(expr)
                                    break
                                elif t.type == "comma" and t.depth == call_start.depth:
                                    args_exprs.append(expr)
                                    expr = []
                                elif t.type != "whitespace":
                                    expr.append(t)
                            
                            args_values = []
                            for ae in args_exprs:
                                if not ae:
                                    args_values.append(None)
                                else:
                                    args_values.append(MI.EvalExpr(ae, GLOBALS, GLOBALS["LINES"]))
                            
                            old_vars = GLOBALS["VARIABLES"].copy()
                            old_globals_vars = GLOBALS["VARIABLES"]
                            GLOBALS["VARIABLES"] = {}
                            old_in_func = GLOBALS.get("_in_function", False)
                            retval = None
                            
                            try:
                                class_type = f"INSTANCE_OF_{var.value.class_def.name}"
                                MI.SetVariable(GLOBALS, "this", var.value, True, class_type, token.line, token.char_pos, token)
                                
                                for idx, p in enumerate(method.parameters):
                                    pname = p.name
                                    ptype = p.type or "*"
                                    
                                    if idx < len(args_values) and args_values[idx] is not None:
                                        aval = args_values[idx]
                                    else:
                                        if hasattr(p, "default") and not isinstance(p.default, type(MI.NotNull())):
                                            aval = p.default
                                        else:
                                            MI.ThrowRuntimeError(f"Missing argument for parameter '{pname}'", token, GLOBALS)
                                    
                                    MI.SetVariable(GLOBALS, pname, aval, False, ptype, token.line, token.char_pos, token)
                                
                                body_tokens = method.body.copy()
                                body_tokens.append(MI.Token("EOF", None, token.line, token.char_pos))
                                
                                current_var_type = VAR_TYPE
                                current_make_const = MAKE_CONST
                                old_depth = DEPTH
                                
                                GLOBALS["_in_function"] = True
                                try:
                                    VAR_TYPE = "VOID"
                                    MAKE_CONST = False
                                    Run(body_tokens)
                                except ReturnException as rexc:
                                    retval = rexc.value
                                finally:
                                    VAR_TYPE = current_var_type
                                    MAKE_CONST = current_make_const
                                    DEPTH = old_depth
                            finally:
                                GLOBALS["VARIABLES"] = old_globals_vars
                                GLOBALS["_in_function"] = old_in_func
                            
                            if VAR_TYPE != "VOID":
                                if retval is not None:
                                    converted_val, success = MI.Convert(retval, VAR_TYPE, True, token.line, token.char_pos, GLOBALS, token)
                                    if success:
                                        MI.SetVariable(GLOBALS, token.aux, converted_val, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, token)
                                    else:
                                        MI.ThrowTypeError(f"Cannot convert method return value to {VAR_TYPE}", token, GLOBALS)
                                else:
                                    MI.SetVariable(GLOBALS, token.aux, None, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, token)
                            else:
                                retval = None
                            
                            i = MI.ClearWhitespace(Tokens, i)
                            if i >= len(Tokens):
                                MI.ThrowSyntaxError("Expected ';' after method call", token, GLOBALS)
                            
                            sem_tok = Tokens[i]
                            i += 1
                            if sem_tok.type != "semicolon":
                                MI.ThrowSyntaxError("Expected ';' after method call", sem_tok, GLOBALS)
                            
                            VAR_TYPE = "VOID"
                            MAKE_CONST = False
                            continue
                        
                        else:
                            MI.ThrowNameError(f"Method '{member_name}' not found in class", member_token, GLOBALS)
                    
                    elif next_member_token.type == "assignment_operator":
                        i += 1
                        
                        if hasattr(var.value, 'attributes') and member_name in var.value.attributes:
                            expr = []
                            while i < len(Tokens):
                                i = MI.ClearWhitespace(Tokens, i)
                                if i >= len(Tokens):
                                    break
                                
                                next_token2 = Tokens[i]
                                if next_token2.type == "semicolon":
                                    val = MI.EvalExpr(expr, GLOBALS, GLOBALS["LINES"])
                                    prop_info = var.value.class_def.properties.get(member_name)
                                    if prop_info:
                                        prop_type, _ = prop_info
                                        val, success = MI.Convert(val, prop_type, True, token.line, token.char_pos, GLOBALS, token)
                                    var.value.attributes[member_name] = val
                                    break
                                expr.append(next_token2)
                                i += 1
                            
                            i += 1
                            continue
                        else:
                            MI.ThrowNameError(f"Property '{member_name}' not found in instance", member_token, GLOBALS)
                    
                    else:
                        if hasattr(var.value, 'attributes') and member_name in var.value.attributes:
                            prop_value = var.value.attributes[member_name]
                            if VAR_TYPE != "VOID":
                                MI.SetVariable(GLOBALS, token.aux, prop_value, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, token)
                                VAR_TYPE = "VOID"
                                MAKE_CONST = False
                            continue
                        else:
                            MI.ThrowNameError(f"Property '{member_name}' not found in instance", member_token, GLOBALS)
                
                elif next_token.type == "assignment_operator":
                    expr: list[MI.Token] = []
                    val: any = None
                    
                    while i < len(Tokens):
                        i = MI.ClearWhitespace(Tokens, i)
                        if i >= len(Tokens):
                            break

                        next_token2 = Tokens[i]
                        
                        if next_token2.type == "semicolon":
                            is_function_call = False
                            func_name_token = None

                            for idx, tok in enumerate(expr):
                                if (tok.type == "identifier" and 
                                    tok.aux in GLOBALS["FUNCTIONS"]):
                                    for j in range(idx + 1, len(expr)):
                                        next_tok = expr[j]
                                        if next_tok.type == "whitespace":
                                            continue
                                        elif next_tok.type == "left_parenthesis":
                                            is_function_call = True
                                            func_name_token = tok
                                            break
                                        else:
                                            break
                                    if is_function_call:
                                        break

                            if is_function_call and func_name_token:
                                func = GLOBALS["FUNCTIONS"][func_name_token.aux]
                                
                                args_exprs = []
                                current_expr = []
                                depth = 0
                                in_args = False
                                found_opening_paren = False

                                for idx, tok in enumerate(expr):
                                    if tok.type == "left_parenthesis" and not found_opening_paren:
                                        found_opening_paren = True
                                        in_args = True
                                        continue
                                    
                                    if not found_opening_paren:
                                        continue
                                        
                                    if tok.type == "right_parenthesis" and depth == 0 and in_args:
                                        if current_expr:
                                            args_exprs.append(current_expr)
                                        break
                                    elif tok.type == "comma" and depth == 0 and in_args:
                                        if current_expr:
                                            args_exprs.append(current_expr)
                                            current_expr = []
                                    else:
                                        if tok.type == "left_parenthesis":
                                            depth += 1
                                        elif tok.type == "right_parenthesis":
                                            depth -= 1
                                        
                                        if in_args and tok.type != "whitespace":
                                            current_expr.append(tok)

                                args_values = []

                                for ae in args_exprs:
                                    if not ae:
                                        args_values.append(None)
                                    else:
                                        args_values.append(MI.EvalExpr(ae, GLOBALS, GLOBALS["LINES"]))

                                old_vars = GLOBALS["VARIABLES"].copy()
                                old_globals_vars = GLOBALS["VARIABLES"]
                                GLOBALS["VARIABLES"] = {}
                                old_in_func = GLOBALS.get("_in_function", False)
                                retval = None

                                try:
                                    for idx, p in enumerate(func.parameters):
                                        pname = p.name
                                        ptype = p.type or "*"
                                        
                                        if idx < len(args_values) and args_values[idx] is not None:
                                            aval = args_values[idx]
                                        else:
                                            if hasattr(p, "default") and not isinstance(p.default, type(MI.NotNull())):
                                                aval = p.default
                                            else:
                                                MI.ThrowRuntimeError(f"Missing argument for parameter '{pname}'", func_name_token, GLOBALS)

                                        MI.SetVariable(GLOBALS, pname, aval, False, ptype, func_name_token.line, func_name_token.char_pos, func_name_token)

                                    body_tokens = func.body.copy()
                                    body_tokens.append(MI.Token("EOF", None, func_name_token.line, func_name_token.char_pos))
                                    
                                    current_var_type = VAR_TYPE
                                    current_make_const = MAKE_CONST
                                    old_depth = DEPTH
                                    
                                    GLOBALS["_in_function"] = True
                                    try:
                                        VAR_TYPE = "VOID"
                                        MAKE_CONST = False
                                        Run(body_tokens)
                                    except ReturnException as rexc:
                                        retval = rexc.value
                                    except KeyboardInterrupt:
                                        MI.ThrowKeyboardInterruptError("'CONTROL' + 'C' keys pressed, interrupting program...", CURRENT_TOKEN, GLOBALS)
                                    finally:
                                        tret = type(retval).__name__.upper() if type(retval).__name__.upper() != "NONETYPE" else "VOID"
                                        tretf = func.type.upper()
                                        if tret != tretf:
                                            MI.ThrowTypeError(f"Cannot return {type(retval).__name__.upper()} through {func.type.upper()} function", CURRENT_TOKEN, GLOBALS)

                                        VAR_TYPE = current_var_type
                                        MAKE_CONST = current_make_const
                                        DEPTH = old_depth
                                finally:
                                    GLOBALS["VARIABLES"] = old_globals_vars
                                    GLOBALS["_in_function"] = old_in_func

                                if retval is not None:
                                    converted_val, success = MI.Convert(retval, VAR_TYPE, True, token.line, token.char_pos, GLOBALS, func_name_token)
                                    if success:
                                        val = converted_val
                                    else:
                                        MI.ThrowTypeError(f"Cannot convert function return value to {VAR_TYPE}", func_name_token, GLOBALS)
                                else:
                                    val = None
                                    
                                MI.SetVariable(GLOBALS, token.aux, val, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, func_name_token)
                            else:
                                try:
                                    #print("> "+"\n> ".join([str(t) for t in expr]))
                                    val = MI.EvalExpr(expr, GLOBALS, GLOBALS["LINES"])
                                except Exception as e:
                                    val, s = MI.Convert("".join([str(t.aux) for t in expr]), VAR_TYPE, False, token.line, token.char_pos, GLOBALS, expr[0] if expr else token)
                                    if not s:
                                        MI.ThrowRuntimeError(f"Error evaluating expression: {str(e)}", expr[0] if expr else token, GLOBALS)
                                
                                MI.SetVariable(GLOBALS, token.aux, val, MAKE_CONST, VAR_TYPE, token.line, token.char_pos, expr[0] if expr else token)
                            
                            break

                        expr.append(next_token2)
                        i += 1
                    
                    VAR_TYPE = "VOID"
                    MAKE_CONST = False
                    continue
                
                else:
                    MI.GetVariable(GLOBALS, token.aux, token)
                    continue


def ConsoleMode():
    GLOBALS["FILE"] = __file__
    GLOBALS["FILE_PATHS"] = [__file__]
    now = MI.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    platform = MI.sys.platform
    plat = "Unknown"
    plats = {"win": "Windows", "cygwin": "Windows", "msys": "Windows", "darwin": "MacOS", "linux": "Linux", "os": "OS",
             "riscos": "RiscOS", "atheos": "AtheOS", "freebsd": "FreeBSD", "openbsd": "OpenBSD", "aix": "AIX"}

    for k in plats.keys():
        if platform.startswith(k):
            plat = plats[k]
            break

    print(f"MowLang v{LanguageInfo.VERSION} (BUILD {LanguageInfo.BUILD}) ({now}) [platform: {plat} || sys: {platform} || type: {MI.os.name}]")
    while True:
        line = ' '
        try:
            line = input("> ")
            GLOBALS["LINES"] = [line]
            Tokens = Tokenize(line)
            Run(Tokens)
        except KeyboardInterrupt:
            t = MI.Token("empty", ' ', 1, 1, 0, ' ')
            MI.ThrowKeyboardInterruptError("Keyboard forced interruption.", t, {"FILE": __file__, "LINES": [line]})

def Main():
    global LINES, CURRENT_TOKEN, GLOBALS
    if len(GLOBALS["FILE_PATHS"]) < 1:
        ConsoleMode()
        quit(0)
    try:
        for FILE in GLOBALS["FILE_PATHS"]:
            FILE = MI.os.path.abspath(FILE)
            FILE = OpenFile(FILE)
            LINES = FILE.splitlines()
            GLOBALS["LINES"] = LINES
            Tokens = Tokenize(FILE)
            Run(Tokens)
    except KeyboardInterrupt:
        MI.ThrowKeyboardInterruptError("Keyboard forced interruption.", CURRENT_TOKEN, GLOBALS)

if __name__ == "__main__":
    Main()