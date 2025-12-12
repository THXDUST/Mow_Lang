import sys, os, time, datetime, codecs, packaging #, Convert_OLD
from packaging.version import parse, Version, InvalidVersion

#################################################################
# TODO: TENTAR REFERENCIAR UMA FUNÇÃO EXTERNA PELO IMPORT       #
#       RESULTA EM UMA REFERÊNCIA NO CÓDIGO LOCAL, OU SEJA      #
#       MESMO QUE IMPORTE UMA BIBLIOTECA PARA USAR SUAS         #
#       FUNÇÕES, O INTERPRETADOR NÃO SABE DIFERENCIAR FUNÇÕES   #
#       LOCAIS DE FUNÇÕES ESTRANGEIRAS                          #
#                                                               #
# DONE: PARA ARRUMAR O PROBLEMA ACIMA, JÁ APLIQUEI UMA PRO-     #
#       PRIEDADE DE ARQUIVO NO CONSTRUTOR DE FUNÇÕES, RESTA     #
#       APENAS FAZER ELE ABRIR O ARQUIVO REFERENCIADO NESSA     #
#       PROPRIEDADE E PUXAR A FUNÇÃO ANTES DE TENTAR EXECU-     #
#       TAR                                                     #
#################################################################

global currentFile

__DEVMODE__ = 1 == 0
__CONSOLE_MODE__ = False
__CURRENT_VERSION__ = Version("1.0.0")
__TIMER__ = None
__TO_PRERUN__ = ["#DEFINE", "LABEL", "IMPORT"]
insideFunc = 0
recentOpenFuncs = []
lineBeforeCallingFunc = -1
shouldRunFuncCode = False
data_types = ["str", "int", "float", "bool", "char", "list", "dict", "null"]
currentFile = "local"

__GLOBALS__ = {
    "__NAME__": __name__,
    "__PROGRAM_FILEPATH__": "",
    "__DEBUG__": False,
    "__FILES__": [],
    "__LABELS__": {},
    "__VARIABLES__": {},
    "__FUNCTIONS__": {},
    "__FILE_LINES__": [],
    "__STACK__": None,
    "__IGNORE_VERSION_WARNS__": False,
    "__DEFAULT_READ_TEXT__": "",
    "__MIN_VER__": Version("0.0.1"),
    "__OS_NAME__": os.name,
    "__PLATFORM__": sys.platform
}

class Var:
    # Integer, Float, String, Boolean, List, Dictionary, Character

    def __init__(self, ref_name, typeof, value):
        self.ref_name = ref_name
        self.typeof = typeof
        self.value = value

class Stack:
    def __init__(self, size, cline):
        if size < 1:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT ALLOCATE STACK WITH SIZE OF {size}", line=cline)
        if __GLOBALS__["__DEBUG__"]:
            print(f"ALLOCATING STACK WITH SIZE OF {size}")
        self.buffer = [0 for _ in range(size)]
        self.pointer = -1
        self.size = size

    def push(self, value, cline):
        try:
            self.pointer += 1
            if __GLOBALS__["__DEBUG__"]:
                print("PUSH -> POINTER:", self.pointer, "VALUE:", value)
            self.buffer[self.pointer] = value
            return self.buffer[self.pointer]
        except IndexError:
            return __DEBUG_INFO__(level="CRITICAL", write_to_file=True, close_program=True, message="STACK OVERFLOW, MAKE SURE TO ALLOCATE MORE MEMORY TO THE PROGRAM", line=cline)

    def pop(self, cline):
        try:
            value = self.buffer[self.pointer]
            self.pointer-=1
            return value
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="INDEX OUT OF RANGE FOR TYPE STACK", line=cline)
        except Exception as e:
            return __DEBUG_INFO__("CRITICAL", True, True, f"{str(e.with_traceback).upper()}\n\t{str(e).upper()}", cline)

    def top(self):
        return self.buffer[self.pointer]

    def print(self):
        print(self.buffer[:self.pointer + 1])
        return self.buffer[:self.pointer + 1]

    def printall(self):
        print(self.buffer)
        return self.buffer

    def clear(self):
        while self.pointer > -1:
            self.buffer[self.pointer] = 0
            self.pointer -= 1
        return 0

    def destroy(self):
        del self.buffer

class Func:
    def __init__(self, name:str, refline:int, args:list, file:str):
        self.args={}
        self.name = name
        self.line = refline
        self.file = file
        for i, arg in enumerate(args):
            self.args[f"arg{i}"] = arg
        #print(self.args)

def __DEBUG_INFO__(level="WARN", write_to_file=False, close_program=False, message="Verify language developer, no argument provided to logging this error", line=-1, exit_code=-1, locals=__GLOBALS__):
    global currentFile
    writable = f"TRACING RECENT CALL:\n\t[{level}] {message}\n\tKEYWORD: {locals['__FILE_LINES__'][line-1].strip()}\n\tAT LINE: {line}\n\tINSIDE FILE: {currentFile}\n" if not __CONSOLE_MODE__ else f"TRACING RECENT CALL:\n\t[{level}] {message}\n\tKEYWORD: {locals['__FILE_LINES__'][-1]}\n\tAT LINE: 1\n"
    if write_to_file and not __CONSOLE_MODE__:
        with open(locals["__PROGRAM_FILEPATH__"].split("/")[-1] + ".log", "a") as f:
            f.write(writable + "\n")
    print("\n\n"+writable)
    if close_program:
        print(f"PROGRAM LEFT WITH EXIT CODE: {exit_code}")
        return ["leave", False]

for arg in sys.argv:
    if arg.endswith(".mow"):
        __GLOBALS__["__FILES__"].append(arg)
    elif arg.lower() == "debug":
        __GLOBALS__["__DEBUG__"] = True

if len(__GLOBALS__["__FILES__"]) == 0:
    __CONSOLE_MODE__ = True

def __IS_STRING__(literal:str, offset=0):
    try:
        parts = literal.split(None, 1+offset)
    except AttributeError:
        return [literal, False]
    if len(parts) <= 1+offset:
        return [literal, False]
    lit = parts[1+offset].rstrip("\n")
    lit = codecs.decode(lit, "unicode_escape")
    is_string = (lit.startswith('"') and lit.endswith('"')) or (lit.startswith("'") and lit.endswith("'"))
    return [lit, is_string]

def __DICT_LIST_PARSER__(value:str, typeof:str, cline:int, locals:dict, error:bool=True):
    def parse_container(val, locals, is_dict=False):
        parts = []
        buf = []
        in_str = False
        str_char = ""
        esc = False
        brace_depth = bracket_depth = paren_depth = 0

        for c in val:
            if in_str:
                buf.append(c)
                if esc:
                    esc = False
                elif c == "\\":
                    esc = True
                elif c == str_char:
                    in_str = False
            else:
                if c in ('"', "'"):
                    in_str = True
                    str_char = c
                    buf.append(c)
                elif c == "{":
                    brace_depth += 1
                    buf.append(c)
                elif c == "}":
                    brace_depth -= 1
                    buf.append(c)
                elif c == "[":
                    bracket_depth += 1
                    buf.append(c)
                elif c == "]":
                    bracket_depth -= 1
                    buf.append(c)
                elif c == "(":
                    paren_depth += 1
                    buf.append(c)
                elif c == ")":
                    paren_depth -= 1
                    buf.append(c)
                elif c == "," and brace_depth == 0 and bracket_depth == 0 and paren_depth == 0:
                    part = "".join(buf).strip()
                    if part:
                        if is_dict:
                            parts.append(part)
                        else:
                            valpart, succ = __CONVERT_TO__(part, "*", cline, False, locals)
                            parts.append(valpart if succ else part)
                    buf = []
                else:
                    buf.append(c)

        tail = "".join(buf).strip()
        if tail:
            if is_dict:
                parts.append(tail)
            else:
                valpart, succ = __CONVERT_TO__(tail, "*", cline, False, locals)
                parts.append(valpart if succ else tail)

        return parts

    def parse_dict(val, locals):
        raw_val = val
        str_val = val[1:-1].strip()
        parts = parse_container(str_val, locals, is_dict=True)

        final = {}
        for part in parts:
            try:
                keypart, valuepart = part.split(":", 1)
            except ValueError:
                if error:
                    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True,
                                          message="ERROR WHILE PARSING TYPE DICT. KEYS MUST BE SEPARATED FROM VALUES WITH ':'",
                                          line=cline)
                else:
                    return [raw_val, False]

            keypart = keypart.strip()
            valuepart = valuepart.strip()

            keyliteral, keyis_string = __IS_STRING__(keypart, -1)
            if not keyis_string:
                if error:
                    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True,
                                          message=f"ERROR WHILE PARSING TYPE DICT. KEY {keypart} MUST BE A STRING", line=cline)
                else:
                    return [raw_val, False]

            keyliteral = keyliteral[1:-1]

            if valuepart == "None":
                valueliteral = None
            else:
                valueliteral, succ = __CONVERT_TO__(valuepart, "*", cline, False, locals)
                if not succ:
                    if error:
                        return __DEBUG_INFO__("CRITICAL", True, True, f"TRIED TO INSERT INVALID VALUE {valuepart} INTO DICT", cline)
                    else:
                        return [raw_val, False]

            final[keyliteral] = valueliteral

        return [final, True]

    if typeof == "dict":
        return [parse_dict(value, locals), True]
    else:
        return [parse_container(value, locals), True]

def __CONVERT_TO__(value, target_type, cline=-1, error:bool=True, locals:dict={}):
    target_type = target_type.lower()

    def parse_dict(str_val: str, error: bool = True):
        raw_val = str_val
        if not (str_val.startswith("{") and str_val.endswith("}")):
            if error:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"ERROR WHILE PARSING TYPE DICT. VALUE {str_val} IS NOT A DICT TYPE", line=cline)
            else:
                return [raw_val, False]

        return __DICT_LIST_PARSER__(str_val, "dict", cline, error)

    def parse_num(str_val:str):
        returnval = 0
        success = True
        typeof = None
        try:
            floatval = float(str_val)
            if int(floatval) != floatval:
                returnval = floatval
                typeof = "float"
            else:
                returnval = int(floatval)
                typeof = "int"
            success = True
        except ValueError:
            returnval = str_val
            success = False
            typeof = None
        except TypeError:
            returnval = str_val
            success = False
            typeof = None

        return returnval, success, typeof

    def parse_list(list_val:str, error:bool = True):
        raw_val = list_val
        if type(list_val) == str:
            if list_val.startswith("[") and list_val.endswith("]"):
                list_val = list_val[1:-1]
            else:
                if error:
                    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"ERROR WHILE PARSING TYPE LIST. TYPE UNKNOWN IS NOT LIST", line=cline)
                else:
                    return [raw_val, False]
        elif type(list_val) == list:
            final = []
            for i, val in enumerate(list_val):
                value, succ = __CONVERT_TO__(val, "*", cline, error, locals)
                final.insert(i, value)

        return __DICT_LIST_PARSER__(list_val, "list", cline, locals, error)



    if target_type == "str":
        literal, is_string = __IS_STRING__(value, -1)
        print(literal, is_string)
        if is_string:
            literal = literal[1:-1]
        elif error:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"ERROR WHILE PARSING TYPE STRING. VALUE {value} IS NOT A STRING TYPE", line=cline)
        else:
            return [value, False]
        return [literal, True]
    elif target_type == "int":
        literal, is_num, typeof = parse_num(value)
        if not is_num:
            if error:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"ERROR WHILE PARSING TYPE INT. VALUE {value} IS NOT AN INTEGER/FLOAT TYPE", line=cline)
            else:
                return [value, False]
        return [int(literal), True]
    elif target_type == "float":
        literal, is_num, typeof = parse_num(value)
        if not is_num:
            if error:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"ERROR WHILE PARSING TYPE FLOAT. VALUE {value} IS NOT AN INTEGER/FLOAT TYPE", line=cline)
            else:
                return [value, False]
        return [float(literal), True]
    elif target_type == "bool":
        return [value.lower() in ["true", "y", "yes", "on"], True]
    elif target_type == "char":
        literal, is_string = __IS_STRING__(value, -1)
        if is_string:
            literal = literal[1:-1]
        elif error:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"ERROR WHILE PARSING TYPE CHAR. VALUE {value} IS NOT A CHAR TYPE", line=cline)
        else:
            return [value, False]
        char = literal[0]
        return [char, True]
    elif target_type == "list":
        val, succ = parse_list(value, error)
        return [val, True]
    elif target_type == "dict":
        val, succ = parse_dict(value, error)
        return [val, True]
    elif target_type == "*":
        val, succ, typeof = parse_num(value)
        if succ:
            return [val, succ]
        val, succ = parse_list(value, False)
        if succ:
            return [val, succ]
        val, succ = parse_dict(value, False)
        if succ:
            return [val, succ]
        val, succ = __IS_STRING__(val, -1)
        if succ:
            return [val[1:-1], succ]
        else:
            return __RUN__(val, cline, locals, False)

    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"TYPE {target_type} NOT SUPPORTED OR VALUE INVALID: {value}", line=cline)

def __OPEN_FILE__(filepath):
    try:
        with open(filepath, "r") as f:
            data = f.readlines()
        return [data, True]
    except FileNotFoundError:
        return __DEBUG_INFO__("CRITICAL", True, True, f"CANNOT FIND PATH {filepath}, VERIFY IF THERE WASN'T ANY TYPOS")

def __PARSE_FUNC__(maybefunc: str, warn: bool = True, cline: int = -1, locals:dict={}):
    s = maybefunc.strip()

    pos = s.find("(")
    funcname = s[:pos].strip() if pos != -1 else False
    if not funcname:
        if warn:
            #print("cannot find func name")
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT FIND FUNCTION NAME", cline)
        return [False, False]

    try:
        func = locals["__FUNCTIONS__"][funcname]
    except KeyError:
        if warn:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT FIND FUNCTION NAMED {funcname}. FUNCTION NOT DECLARED ON THE CURRENT SCOPE", cline)
        return [False, False]

    i = pos + 1
    depth_paren = 1
    in_str = False
    str_char = ""
    esc = False
    inner_chars = []

    while i < len(s):
        c = s[i]
        if in_str:
            inner_chars.append(c)
            if esc:
                esc = False
            elif c == "\\":
                esc = True
            elif c == str_char:
                in_str = False
        else:
            if c in ('"', "'"):
                in_str = True
                str_char = c
                inner_chars.append(c)
            elif c == "(":
                depth_paren += 1
                inner_chars.append(c)
            elif c == ")":
                depth_paren -= 1
                if depth_paren == 0:
                    break
                inner_chars.append(c)
            else:
                inner_chars.append(c)
        i += 1

    args_str = "".join(inner_chars)

    args = []
    buf = []
    in_str = False
    str_char = ""
    esc = False
    brace_depth = 0
    bracket_depth = 0
    paren_depth = 0

    for c in args_str:
        if in_str:
            buf.append(c)
            if esc:
                esc = False
            elif c == "\\":
                esc = True
            elif c == str_char:
                in_str = False
        else:
            if c in ('"', "'"):
                in_str = True
                str_char = c
                buf.append(c)
            elif c == "{":
                brace_depth += 1
                buf.append(c)
            elif c == "}":
                brace_depth -= 1
                buf.append(c)
            elif c == "[":
                bracket_depth += 1
                buf.append(c)
            elif c == "]":
                bracket_depth -= 1
                buf.append(c)
            elif c == "(":
                paren_depth += 1
                buf.append(c)
            elif c == ")":
                paren_depth -= 1
                buf.append(c)
            elif c == "," and brace_depth == 0 and bracket_depth == 0 and paren_depth == 0:
                # fim de um argumento
                arg = "".join(buf).strip()
                if arg != "":
                    args.append(arg)
                buf = []
            else:
                buf.append(c)

    tail = "".join(buf).strip()
    if tail != "":
        args.append(tail)

    return [func, args]

def __RUN__(__LINE__:str, cline:int, locals:dict, warn: bool = True, prerunning=False, *kargs):
    # SETTING THINGS UP
    global __GLOBALS__, insideFunc, recentOpenFuncs, lineBeforeCallingFunc, shouldRunFuncCode, pid, currentFile

    with open("console.log", "a") as f:
        f.write(f"\nRunning {__LINE__} in {currentFile}")
    __RAW_LINE__ = __LINE__
    if __LINE__.endswith("\n"):
        __LINE__ = __LINE__[:-1]
    else:
        __RAW_LINE__ = __RAW_LINE__ + "\n"

    # ACTUALLY RUNNING FILE CODE
    # PRINTS TO CONSOLE
    if __LINE__.startswith("PRINT"):
        __LINE__= __LINE__[5:]
        nextline = __LINE__[0] == "L"
        methline = __LINE__
        __LINE__ = __LINE__[1:]
        if len(__LINE__.split()[0]) == 0:
            return __DEBUG_INFO__("ERROR", True, True, "CANNOT PRINT NOTHING", cline)
        literal, is_string = __IS_STRING__(__RAW_LINE__)
        method = False
        val = None
        if len(methline.split()[0].split("."))>1:
            method = methline.split(".")[1].strip("\n")
            if method == "VARS":
                prints = []
                for name in locals["__VARIABLES__"]:
                    ref_name = locals["__VARIABLES__"][name].ref_name
                    typeof = locals["__VARIABLES__"][name].typeof
                    value = locals["__VARIABLES__"][name].value
                    prints.append(f"|{typeof.upper()}| {ref_name}: {value}")
                val = ("\n" if nextline else " || ").join(prints)
                print(val, end=("\n" if nextline else ""))
        else:
            if is_string:
                print(literal[1:-1], end=("\n" if nextline else ""))
            else:
                val, succ = __RUN__(literal, cline, locals)
                print(val, end=("\n" if nextline else ""))
        return [val, True] if val else [True, True]

    # MANAGE STACK OPERATIONS
    elif __LINE__.startswith("STACK"):
        __LINE__ = __LINE__[6:]
        try:
            method = __LINE__.split()[0]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="PROVIDE METHOD WHEN USING STACK COMMAND", line=cline)
        if method == "ALLOC":
            __LINE__= __LINE__[5:]
            try:
                arg = __LINE__.split()
                if arg[0] == '':
                    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO PROVIDE STACK SIZE ARGUMENT TO ALLOCATE MEMORY", line=cline)
                size = int(arg[0])
                locals["__STACK__"] = Stack(size, cline)
                return [True, True]
            except IndexError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO PROVIDE STACK SIZE ARGUMENT TO ALLOCATE MEMORY", line=cline)
            except ValueError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE THAT ARGUMENT PROVIDED TO ALLOCATE MEMORY IS A POSITIVE INTEGER TYPE", line=cline)
            except Exception as e:
                return __DEBUG_INFO__(level="CRITICAL", write_to_file=True, close_program=True, message=f"ERROR NOT TREATED CORRECTLY, CONTACT LANGUAGE DEVELOPER FOR MORE INFO ALONG WITH THE ERROR:\n\t{str(e.with_traceback)}\n\t{str(e)}", line=cline)
        elif method == "PUSH":
            __LINE__ = __LINE__[5:]

            datatype, raw_value = __LINE__.split(" ", 1)
            datatype = datatype.lower()

            try:
                if int(raw_value) != float(raw_value):
                    raw_value = float(raw_value)
                else:
                    raw_value = int(raw_value)
            except ValueError:
                raw_value, succ = __RUN__(raw_value, cline, locals, False)
                if not succ:
                    raw_value, succ = __CONVERT_TO__(raw_value, datatype, cline, True, locals)

                if not succ:
                    try:
                        locals["__STACK__"].push(raw_value, cline)
                    except AttributeError:
                        return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
                    return [raw_value, True]
                else:
                    try:
                        locals["__STACK__"].push(raw_value, cline)
                    except AttributeError:
                        return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
                    return [raw_value, True]

            try:
                locals["__STACK__"].push(raw_value, cline)
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
            return [raw_value, True]
        elif method == "POP":
            try:
                return [locals["__STACK__"].pop(cline), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline, exit_code=-1)
        elif method == "TOP":
            try:
                return [locals["__STACK__"].top(), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
        elif method == "PRINT":
            try:
                return [locals["__STACK__"].print(), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
        elif method == "PRINTALL":
            try:
                return [locals["__STACK__"].printall(), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
        elif method == "CLEAR":
            try:
                return [locals["__STACK__"].clear(), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
        elif method == "DESTROY":
            try:
                return [locals["__STACK__"].destroy(), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
        elif len(method) == 0:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="PROVIDE METHOD WHEN USING STACK COMMAND", line=cline)
        else:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"STACK METHOD {method} IS NOT DEFINED IN THE CURRENT SCOPE", line=cline)

    # MANAGE LABEL OPERATIONS
    elif __LINE__.startswith("LABEL"):
        __LINE__ = __LINE__[6:]
        label = __LINE__.strip(" ")
        locals["__LABELS__"][label] = cline
        return [cline, True]

    # MANAGE STRING INPUT OPERATIONS
    elif __LINE__.startswith("READ"):
        __LINE__ = __LINE__[5:]
        datatype = __LINE__.split(" ", 1)
        if len(datatype) > 1:
            text = datatype[1]
            datatype = datatype[0]
            text = text[1:-1]
        else:
            text = locals["__DEFAULT_READ_TEXT__"]
            datatype = datatype[0]
        inp = input(text)
        if datatype == "int":
            try:
                inp = int(inp)
            except ValueError:
                return __DEBUG_INFO__(level="ERROR", close_program=True, write_to_file=True, message="MAKE SURE YOU ARE INSERTING A NUMBER", line=cline)
        elif datatype == "float":
            try:
                inp = float(inp)
            except ValueError:
                return __DEBUG_INFO__(level="ERROR", close_program=True, write_to_file=True, message="MAKE SURE YOU ARE INSERTING A NUMBER", line=cline)

        # Integer, Float, String, Boolean, List, Dictionary, Character
        elif datatype == "bool":
            inp = inp.lower() in ["true", "yes", "positive"]
        elif datatype == "char":
            inp = inp[0]
        elif datatype == "list":
            inp = inp.split(", ")
        return [inp, True]

    # SEQUENCE OF COMPARATION OPERATORS
    elif __LINE__.startswith("EQ") or __LINE__.startswith("=="):
        cmp = __LINE__.split(" ", 1)[1]
        try:
            top = locals["__STACK__"].top()
            temptype = None
            temptype1 = None

            try:
                comp2 = float(cmp)
                comp = float(top)
                temptype = float
                temptype1 = float
            except ValueError:
                comp, temptype1 = __CONVERT_TO__(top, "str", cline, True, locals), str
                comp2, temptype =__CONVERT_TO__(cmp, "str", cline, True, locals), str


            #print("COMPARING ==: ", cmp, top, try1, otype1, try12, otype12, comp, comp2, temptype1, temptype, comp == comp2)
            return [comp == comp2, True]
        except Exception as e:
            return [locals["__STACK__"].top() == cmp, True]

    elif __LINE__.startswith("GTE") or __LINE__.startswith(">="):
        cmp = __LINE__.split(" ", 1)[1]
        top, toptype = __CONVERT_TO__(locals["__STACK__"].top(), "float", cline, True, locals), float
        cmp, cmptype = __CONVERT_TO__(cmp, "float", cline, True, locals), float
        temptype1 = None
        temptype = None

        try:
            comp2 = float(cmp)
            comp = float(top)
            temptype = float
            temptype1 = float
        except ValueError:
            comp, temptype1 = __CONVERT_TO__(top, "str", cline, True, locals), str
            comp2, temptype = __CONVERT_TO__(cmp, "str", cline, True, locals), str
            comp, temptype1 = __CONVERT_TO__(comp, "float", cline, True, locals), float
            comp2 = temptype = __CONVERT_TO__(comp2, "float", cline, True, locals), float

        try:
            return [comp >= comp2, True]
        except Exception as e:
            return [False, True]

    elif __LINE__.startswith("LTE") or __LINE__.startswith("<="):
        cmp = __LINE__.split(" ", 1)[1]
        top, toptype = __CONVERT_TO__(locals["__STACK__"].top(), "float", cline, True, locals), float
        cmp, cmptype = __CONVERT_TO__(cmp, "float", cline, True, locals), float
        temptype1 = None
        temptype = None

        try:
            comp2 = float(cmp)
            comp = float(top)
            temptype = float
            temptype1 = float
        except ValueError:
            comp, temptype1 = __CONVERT_TO__(top, "str", cline, True, locals), str
            comp2, temptype =__CONVERT_TO__(cmp, "str", cline, True, locals), str
            comp, temptype1 = __CONVERT_TO__(comp, "float", cline, True, locals), float
            comp2 = temptype = __CONVERT_TO__(comp2, "float", cline, True, locals), float

        try:
            return [comp <= comp2, True]
        except Exception as e:
            return [False, True]

    elif __LINE__.startswith("GT") or __LINE__.startswith(">"):
        cmp = __LINE__.split(" ", 1)[1]
        top, toptype = __CONVERT_TO__(locals["__STACK__"].top(), "float", cline, True, locals)
        cmp, cmptype = __CONVERT_TO__(cmp, "float", cline, True, locals)
        print(cmp, top)
        temptype1 = None
        temptype = None

        try:
            comp2 = float(cmp)
            comp = float(top)
            temptype = float
            temptype1 = float
        except ValueError:
            comp, temptype1 = __CONVERT_TO__(top, "str", cline, True, locals), str
            comp2, temptype = __CONVERT_TO__(cmp, "str", cline, True, locals), str
            comp, temptype1 = __CONVERT_TO__(comp, "float", cline, True, locals), float
            comp2, temptype = __CONVERT_TO__(comp2, "float", cline, True, locals), float

        try:
            return [comp > comp2, True]
        except Exception as e:
            return [False, True]

    elif __LINE__.startswith("LT") or __LINE__.startswith("<"):
        cmp = __LINE__.split(" ", 1)[1]
        top, toptype = __CONVERT_TO__(locals["__STACK__"].top(), "float", cline, True, locals), float
        cmp, cmptype = __CONVERT_TO__(cmp, "float", cline, True, locals), float

        try:
            comp2 = float(cmp)
            comp = float(top)
            temptype = float
            temptype1 = float
        except ValueError:
            comp, temptype1 = __CONVERT_TO__(top, "str", cline, True, locals), str
            comp2, temptype = __CONVERT_TO__(cmp, "str", cline, True, locals), str
            comp, temptype1 = __CONVERT_TO__(comp, "float", cline, True, locals), float
            comp2 = temptype = __CONVERT_TO__(comp2, "float", cline, True, locals), float

        try:
            return [comp < comp2, True]
        except Exception as e:
            return [False, True]

    # MANAGE JUMP OPERATIONS
    elif __LINE__.startswith("JUMP"):
        __LINE__ = __LINE__[4:]
        splits = __LINE__.split()
        if len(splits) > 1:
            try:
                method, cmp, label = splits[0], splits[1], splits[2]
                templine = method + " " + cmp
                result, succ = __RUN__(templine, cline, locals)
                if result:
                    return ["skip_to:"+label, True]
            except IndexError:
                return __DEBUG_INFO__(level="ERROR", close_program=True, write_to_file=True, message=f"MAKE SURE THE JUMP LABEL IS RECIEVING COMPARISON METHOD, COMPARISON VALUE AND STRING LABEL NAME THAT IS NOT EMPTY", line=cline)
        elif len(splits) == 1 and splits[0] != '':
            label = splits[0]
            return ["skip_to:"+label, True]
        else:
            return __DEBUG_INFO__(level="ERROR", close_program=True, write_to_file=True, message=f"MAKE SURE THE JUMP LABEL IS RECIEVING AT LEAST THE LABEL NAME ARGUMENT THAT IS NOT EMPTY", line=cline)

    # SEQUENCE OF ARITHMETIC OPERATORS
    elif __LINE__.startswith("ADD"):
        __LINE__ = __LINE__[4:]
        try:
            arg1, arg2 = __LINE__.split(",")[0].strip(), __LINE__.split(",")[1].strip()
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="ADD KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        if arg1 == "STACK":
            arg1 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg1, -1)
            if not is_string:
                arg1, succ = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
                if not succ:
                    arg1, succ = __RUN__(arg1, cline, locals, False, False)

        if arg2 == "STACK":
            arg2 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg2, -1)
            if not is_string:
                arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
                if not succ:
                    arg2, succ = __RUN__(arg2, cline, locals, False, False)

        num1, arg1type = float(arg1), float
        num2, arg2type = float(arg2), float

        if arg1type not in [int, float, str] or arg2type not in [int, float, str]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT ADD {str(arg1type)} + {str(arg2type)}")
        else:
            num1 = float(arg1)
            num2 = float(arg2)

        try:
            result = num2 + num1
        except TypeError:
            result = f"{num2}" + num1

        if type(result) == str:
            result = '"'+result+'"'
        return [result, True]

    elif __LINE__.startswith("SUB"):
        __LINE__ = __LINE__[4:]
        try:
            arg1, arg2 = __LINE__.split(",",1)[0].strip(), __LINE__.split(",",1)[1].strip()
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="SUB KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        if arg1.upper() == "STACK":
            arg1 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg1, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT SUBTRACT FROM STRING", cline)
            else:
                arg1, succ = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
                if not succ:
                    arg1, succ = __RUN__(arg1, cline, locals, False, False)

        if arg2.upper() == "STACK":
            arg2 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg2, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT SUBTRACT FROM STRING", cline)
            else:
                arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
                if not succ:
                    arg2, succ = __RUN__(arg2, cline, locals, False, False)

        num1, arg1type = float(arg1), float
        num2, arg2type = float(arg2), float

        if arg1type not in [int, float] or arg2type not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT SUBTRACT {str(arg1type)} - {str(arg2type)}", line=cline)
        else:
            num1 = float(arg1)
            num2 = float(arg2)

        result = num2 - num1
        return [result, True]

    elif __LINE__.startswith("MUL"):
        __LINE__ = __LINE__[4:]
        try:
            arg1, arg2 = __LINE__.split(",")[0].strip(), __LINE__.split(",")[1].strip()
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MUL KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        if arg1 == "STACK":
            arg1 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg1, -1)
            if not is_string:
                arg1, succ = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
                if not succ:
                    arg1, succ = __RUN__(arg1, cline, locals, False, False)

        if arg2 == "STACK":
            arg2 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg2, -1)
            if not is_string:
                arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
                if not succ:
                    arg2, succ = __RUN__(arg2, cline, locals, False, False)

        num1, arg1type = float(arg1), float
        num2, arg2type = float(arg2), float

        if arg1type not in [int, float, str] or arg2type not in [int, float, str]:
            __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MULTIPLY {str(arg1type)} * {str(arg2type)}", line=cline)
        else:
            num1 = arg1
            num2 = arg2

        if arg1type == arg2type == str:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MULTIPLY STRING BY STRING", line=cline)
        elif (arg1type == str and int(__CONVERT_TO__(num2, "float", cline, True, locals)[0]) != num2) or (arg2type == str and int(__CONVERT_TO__(num1, "float", cline, True, locals)[0]) != num1):
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MULTIPLY STRING BY FLOATING POINT VALUE", line=cline)

        if not int(__CONVERT_TO__(num1, "float", cline, True, locals)[0]) != num1:
            num1 = int(num1)
        if not int(__CONVERT_TO__(num2, "float", cline, True, locals)[0]) != num2:
            num2 = int(num2)

        try:
            result = num2 * num1
        except TypeError:
            result = num1 * num2

        if type(result) == str:
            result = '"'+result+'"'
        return [result, True]

    elif __LINE__.startswith("DIV"):
        __LINE__ = __LINE__[4:]
        try:
            arg1, arg2 = __LINE__.split(",")[0].strip(), __LINE__.split(",")[1].strip()
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="DIV KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        if arg1 == "STACK":
            arg1 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg1, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT SUBTRACT FROM STRING", cline)
            else:
                arg1, succ = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
                if not succ:
                    arg1, succ = __RUN__(arg1, cline, locals, False, False)

        if arg2 == "STACK":
            arg2 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg2, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT DIVIDE FROM STRING", cline)
            else:
                arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
                if not succ:
                    arg2, succ = __RUN__(arg2, cline, locals, False, False)

        num1, arg1type = float(arg1), float
        num2, arg2type = float(arg2), float

        if arg1type not in [int, float] or arg2type not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT DIVIDE {str(arg1type)} / {str(arg2type)}", line=cline)
        else:
            num1 = arg1
            num2 = arg2

        if not int(__CONVERT_TO__(num1, "float", cline, True, locals)[0]) != num1:
            num1 = int(num1)
        else:
            num1 = float(num1)
        if not int(__CONVERT_TO__(num2, "float", cline, True, locals)[0]) != num2:
            num2 = int(num2)
        else:
            num2 = float(num2)

        if num2 == 0:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT DIVIDE BY 0: {num1} / {num2}", line=cline)

        result = num2 / num1
        return [result, True]

    elif __LINE__.startswith("MOD"):
        __LINE__ = __LINE__[4:]
        try:
            arg1, arg2 = __LINE__.split(",")[0].strip(), __LINE__.split(",")[1].strip()
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MOD KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        if arg1 == "STACK":
            arg1 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg1, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT MODULATE FROM STRING", cline)
            else:
                arg1, succ = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
                if not succ:
                    arg1, succ = __RUN__(arg1, cline, locals, False, False)

        if arg2 == "STACK":
            arg2 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg2, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT MODULATE FROM STRING", cline)
            else:
                arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
                if not succ:
                    arg2, succ = __RUN__(arg2, cline, locals, False, False)

        num1, arg1type = float(arg1), float
        num2, arg2type = float(arg2), float

        if arg1type not in [int, float] or arg2type not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MODULATE {str(arg1type)} % {str(arg2type)}", line=cline)
        else:
            num1 = arg1
            num2 = arg2

        if not int(__CONVERT_TO__(num1, "float", cline, True, locals)[0]) != num1:
            num1 = int(num1)
        if not int(__CONVERT_TO__(num2, "float", cline, True, locals)[0]) != num2:
            num2 = int(num2)

        if num2 == 0:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MODULATE BY 0: {num1} % {num2}", line=cline)

        result = num2 % num1
        return [result, True]

    elif __LINE__.startswith("EXP"):
        __LINE__ = __LINE__[4:]
        try:
            arg1, arg2 = __LINE__.split(",")[0].strip(" "), __LINE__.split(",")[1].strip(" ")
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="EXP KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        if arg1 == "STACK":
            arg1 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg1, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT GET THE POWER FROM STRING", cline)
            else:
                arg1, succ = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
                if not succ:
                    arg1, succ = __RUN__(arg1, cline, locals, False, False)

        if arg2 == "STACK":
            arg2 = locals["__STACK__"].pop(cline)
        else:
            literal, is_string = __IS_STRING__(arg2, -1)
            if is_string:
                return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT SUBTRACT FROM STRING", cline)
            else:
                arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
                if not succ:
                    arg2, succ = __RUN__(arg2, cline, locals, False, False)

        num1, arg1type = float(arg1), float
        num2, arg2type = float(arg2), float

        if arg1type not in [int, float] or arg2type not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT ELEVATE {str(arg1type)} ^ {str(arg2type)}", line=cline)
        else:
            num1 = arg1
            num2 = arg2

        if not int(__CONVERT_TO__(num1, "float", cline, True, locals)[0]) != num1:
            num1 = int(num1)
        if not int(__CONVERT_TO__(num2, "float", cline, True, locals)[0]) != num2:
            num2 = int(num2)

        if num1 == 0:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT ELEVATE 0 BY {num2}: {num1}^{num2}", line=cline)

        result = num2**num1
        return [result, True]

    # VARIABLE OPERATORS
    elif __LINE__.startswith("STORE"):
        #STORE type name (value or Stack.top() if None)
        __LINE__ = __LINE__[6:]
        args = __LINE__.split(" ", 2)

        try:
            typeof, name = args[0].lower(), args[1]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="STORE KEYWORD MUST HAVE VARIABLE TYPE AND VARIABLE NAME", line=cline)

        try:
            value = args[2]
        except IndexError:
            value = locals["__STACK__"].top()

        if typeof not in data_types:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"VARIABLE TYPE MUST BE ONE OF THESE: {data_types}", line=cline)

        literal, is_string = __IS_STRING__(value, -1)
        if (typeof == "str" or typeof == "char") and not (is_string):
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="TYPE STRING MUST BE STRING", line=cline)

        if is_string:
            literal = literal[1:-1]

        if is_string and typeof == "char":
            literal = literal[0]
        elif is_string and typeof in ["int", "float", "bool", "list", "dict"]:
            return __DEBUG_INFO__(level="CRITICAL", write_to_file=True, close_program=True, message=f"TYPE ERROR. TYPE {typeof} CANNOT RECIEVE STRING VALUE", line=cline)

        result, succ = __CONVERT_TO__(f'"{literal}"' if typeof in ["str", "char"] else literal, typeof, cline, True, locals)

        if not succ:
            return [result, succ]

        value = result
        var = Var(name, typeof, value)
        locals["__VARIABLES__"][name] = var

        return [[var.ref_name, var.typeof, var.value], True]

    elif __LINE__.startswith("LOAD"):
        __LINE__ = __LINE__[5:]

        namearg = __LINE__.strip()

        try:
            val = locals["__VARIABLES__"][namearg].value
        except KeyError:
            try:
                val = __GLOBALS__["__VARIABLES__"][namearg].value
            except KeyError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"VARIABLE {namearg} IS NOT DEFINED IN THE CURRENT SCOPE", line=cline)

        return [val, True]

    # PAUSE OPERATOR
    elif __LINE__.startswith("PAUSE"):
        os.system("pause" if locals["__OS_NAME__"] == "nt" else "read -p \"Press any key to continue . . .\"")
        return [True, True]

    # TIME OPERATOR
    elif __LINE__.startswith("TIME"):
        global __TIMER__
        args = __LINE__.split(" ")
        try:
            method = args[0].split(".")[1]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "TIME NEEDS ONE METHOD TO WORK CORRECTLY", cline)

        if method == "SLEEP":
            if len(args) < 2:
                return __DEBUG_INFO__("ERROR", True, True, "TIME.SLEEP NEEDS AT LEAST ONE FLOAT/INTEGER ARGUMENT", cline)
            else:
                try:
                    secs = float(args[1])
                except ValueError:
                    secs = __CONVERT_TO__(args[1], "float", cline, True, locals)
            time.sleep(secs)
            return [secs, True]
        elif method == "START":
            __TIMER__ = time.time()
            return [__TIMER__, True]
        elif method == "STOP":
            if not __TIMER__:
                __DEBUG_INFO__(write_to_file=True, message="TIMER WAS NOT STARTED BEFORE STOPPED", line=cline)
        elif method == "STAMP":
            return [time.time(), True]
        elif method == "NOW":
            now = args[1] if len(args) > 1 else "%Y-%m-%d %H:%M:%S"
            return [datetime.datetime.now().strftime(now), True]
        else:
            return __DEBUG_INFO__("ERROR", True, True, f"TIME METHOD {method} IS NOT DEFINED IN THE CURRENT SCOPE", cline)

    # LIBRARY MANAGER
    elif __LINE__.startswith("IMPORT"):
        args = __LINE__.split()
        if len(args) <= 1:
            return __DEBUG_INFO__("ERROR", True, True, "CANNOT RESOLVE NULL IMPORT", cline)

        file = args[1]
        literal, is_string = __IS_STRING__(file, -1)
        if is_string:
            file = literal[1:-1]+".mow"
        
        try:
            with open(file, "r") as f:
                importlines = f.readlines()
            currentFile = file
            for cmd in __TO_PRERUN__:
                __PRE_RUN__(cmd, importlines, locals)

            __PRE_RUN__("*", importlines, locals, topre=__TO_PRERUN__)
        except FileNotFoundError:
            file = file[:-4]
            try:
                with open(file, "r") as f:
                    importlines = f.readlines()
                currentFile = file
                __PRE_RUN__("LABEL", importlines)
                __PRE_RUN__("*", importlines)
                locals["__STACK__"].destroy()
                currentFile = "local"
            except FileNotFoundError:
                return __DEBUG_INFO__("CRITICAL", True, True, f"CANNOT RESOLVE IMPORT FILE {file} FILE MISSING")
        
        return [True, True]

    # INTERPRETER BEHAVIOUR CONFIGURATOR
    elif __LINE__.startswith("#DEFINE"):
        args = __LINE__.split(" ", 3)
        lar = len(args)

        command = method = arg2 = arg3 = arg4 = None

        if lar == 4:
            method, arg2, arg3 = args[1].strip(), args[2].strip(), args[3].strip()
        elif lar == 3:
            method, arg2 = args[1].strip(), args[2].strip()
        elif lar == 2:
            method = args[1].strip()
        else:
            return __DEBUG_INFO__("ERROR", True, True, "#DEFINE COMMAND NEEDS AT LEAST ONE DEFENITION TO DEFINE SOMETHING")

        if method == "NO_VERSION_WARNS":
            locals["__IGNORE_VERSION_WARNS__"] = True
            return [True, True]
        elif method == "INPUT_PROMPT":
            if arg3:
                arg2 = arg2 + " " + arg3
            literal, is_string = __IS_STRING__(arg2, -1)
            if is_string:
                arg2 = literal[1:-1]
            else:
                return __DEBUG_INFO__("ERROR", True, True, "DEFINITION \"INPUT_PROMPT\" MUST BE A STRING TYPE", cline)
            locals["__DEFAULT_READ_TEXT__"] = arg2
            return [arg2, True]
        elif method == "REQUIRE_VERSION":
            warn = True
            ver2 = None

            for arg in args:
                if arg.upper() in ["-N", "-F", "-NW", "-NOWARN"]:
                    warn = False

                try:
                    ver2 = Version(arg)
                except InvalidVersion:
                    continue

            ver1 = __CURRENT_VERSION__
            ver2 = ver2

            if ver1 < ver2:
                return __DEBUG_INFO__("CRITICAL", True, True, f"CANNOT DEFINE VERSION {ver2} AS MINIMUM VERSION TO RUN BECAUSE THE INTERPRETER'S VERSION IS INFERIOR TO THE SELECTED VERSION ({__CURRENT_VERSION__})", cline)
            elif warn:
                inp = input("SETTING THE INTERPRETER'S MINIMUM FILE VERSION MIGHT CAUSE DEPENDENCIES TO NOT RUN\nDO YOU WANT TO PROCEDE? (Y/N)\n(ADD \"-N\", \"-F\", \"-NW\" OR \"-NOWARN\" TO SILENCE THIS WARNING)\n>> ")
                if inp.upper() == "Y":
                    locals["__MIN_VER__"] = ver2
                else:
                    return "leave"
            else:
                locals["__MIN_VER__"] = ver2
            return [ver2, True]
        elif method == "DEBUG":
            debug = locals["__DEBUG__"]

            if not arg1:
                debug = not debug
            else:
                debug = arg1.upper() in ["ON", "TRUE", "YES", "ACTIVATE", "ACT", "Y"]
            
            locals["__DEBUG__"] = debug
            return [debug, True]
        else:
            return __DEBUG_INFO__("ERROR", True, True, f"#DEFINE ARGUMENT {method} IS NOT DEFINED IN THE CURRENT SCOPE")

    # FUNCTION CREATOR
    elif __LINE__.startswith("FUNCTION"):
        __LINE__ = __LINE__[9:]
        args = __LINE__.split(" ")
        insideFunc+=1
        name = args[0]
        args = args[1:]
        func = Func(name, cline-1, args, currentFile)
        recentOpenFuncs.append(name)
        if not (kargs[0] if len(kargs) > 0 else False):
            locals["__FUNCTIONS__"][name] = func
        return [[name, cline-1, args], True]
    
    # FUNCTION TERMINATOR
    elif __LINE__.startswith("END"):
        name = __LINE__[4:]
        if recentOpenFuncs[-1] != name:
            return __DEBUG_INFO__("CRITICAL", True, True, f"TRIED TO LEAVE FUNCTION {name} BEFORE CLOSING HIS CHILD FUNCTION", cline)
        insideFunc-=1
        recentOpenFuncs.pop()
        if (insideFunc == 0 or len(recentOpenFuncs) <= 0) and shouldRunFuncCode:
            shouldRunFuncCode = False
            recentOpenFuncs = []
            return [f"line:{lineBeforeCallingFunc}", True]
        elif shouldRunFuncCode:
            return [f"line:{lineBeforeCallingFunc}", True]

        return [[recentOpenFuncs, shouldRunFuncCode, cline, lineBeforeCallingFunc, locals["__FILE_LINES__"][lineBeforeCallingFunc]], True]

    # FUNCTION RETURNER
    elif __LINE__.startswith("RETURN"):
        __LINE__ = __LINE__[7:]
        try:
            typeof, value = __LINE__.split(" ", 1)
        except ValueError:
            return [None, True]
        
        lit, is_string = __IS_STRING__(value, -1)
        if not is_string:
            val, succ = __CONVERT_TO__(lit, typeof, cline, False, locals)
        else:
            val = lit
        if not succ:
            val, succ = __RUN__(lit, cline, locals, False, False)
        return [val, True]

    # LEAVE THE CODE
    elif __LINE__.startswith("EXIT"):
        return ["leave", True]

    # RAISE ERROR IF __RUN__ NOT CALLED INTERNALLY BY INTERPRETER
    else:
        func:Func = None
        args:list = []
        fargs:dict = {}
        func, args = __PARSE_FUNC__(__LINE__, False, cline, locals)
        if not args:
            func, args2 = __PARSE_FUNC__(__LINE__, False, cline, __GLOBALS__)

        if args:
            lineBeforeCallingFunc = cline
            for i, arg in enumerate(args):
                arg, succ = __CONVERT_TO__(arg,  "*", cline, True, locals)
                fargs[f"arg{i}"] = arg

            shouldRunFuncCode = True

            return __RUN_FUNC__(fargs, func, locals)
        elif args2:
            lineBeforeCallingFunc = cline
            for i, arg in enumerate(args2):
                arg, succ = __CONVERT_TO__(arg,  "*", cline, True, locals)
                fargs[f"arg{i}"] = arg

            shouldRunFuncCode = True

            return __RUN_FUNC__(fargs, func, locals)
        
        try:
            return [locals[__LINE__], True]
        except KeyError:
            pass
        

        if warn:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, message=f"KEYWORD {__LINE__.split(' ')[0]} IS NOT DEFINED", line=cline)
        return [__LINE__, False]

def __PRE_RUN__(command:str, lines:list, locals:dict, optline:int=0, topre:list=[]):
    tmp = optline
    status = None
    while tmp < len(lines):
        line = lines[tmp]
        line = line.split("-->")[0]

        if line.startswith(command) or (command == "*" and not line.startswith("-->") and line != "\n" and not line.startswith("#DEFINE")) and not (line in topre):
            status, succ = __RUN__(line, tmp+1, locals, prerunning=True)

        if status == "leave":
            break
        elif str(status).startswith("skip_to:"):
            status = status[8:]
            try:
                tmp = locals["__LABELS__"][status]
                if __DEVMODE__:
                    print(f"SKIPPING TO LABEL {status} (LINE: {locals['__LABELS__'][status]})")
                continue
            except KeyError:
                __DEBUG_INFO__(level="WARN", write_to_file=True, message=f"LABEL {status} IS NOT DEFINED IN THE CURRENT SCOPE, CONTINUING PROGRAM", line=tmp+1)

        tmp+=1

def __RUN_FUNC__(args:dict, func:Func, locals:dict):
    global __TO_PRERUN__
    __LOCALS__ = {
        "__NAME__": __GLOBALS__["__NAME__"],
        "__VARIABLES__": {},
        "__DEBUG__": __GLOBALS__["__DEBUG__"],
        "__LABELS__": {},
        "__FUNCTIONS__": {},
        "__STACK__": None,
        "__DEFAULT_READ_TEXT__": "",
        "__PLATFORM__": __GLOBALS__["__PLATFORM__"],
        "__IGNORE_VERSION_WARNS__": __GLOBALS__["__IGNORE_VERSION_WARNS__"],
        "__MIN_VER__": __GLOBALS__["__MIN_VER__"],
        "__OS_NAME__": __GLOBALS__["__OS_NAME__"]
    }

    funcArgs = func.args
    lines:list = __OPEN_FILE__(func.file)[0]
    funcReturn:any = None
    cline:int = func.line

    la, lf = len(args), len(funcArgs)


    if la != lf:
        return __DEBUG_INFO__("CRITICAL", True, True, f"TRIED TO INSERT {'ONLY ' if la < lf else ''}{la} {'ARGUMENTS' if abs(la) != 1 else 'ARGUMENT'} INTO FUNCTION WITH {'ONLY ' if lf < la else ''}{lf} {'ARGUMENTS' if abs(lf) != 1 else 'ARGUMENT'}", lineBeforeCallingFunc)

    for name, val in args.items():
        __LOCALS__["__VARIABLES__"][funcArgs[name]] = Var(funcArgs[name], type(val), val)

    for command in __TO_PRERUN__:
        __PRE_RUN__(command, lines, __LOCALS__, cline)

    while cline < len(lines):
        try:
            line:str = lines[cline]
            status = ""

            isprerun = False

            try:
                isprerun = line.split()[0] in __TO_PRERUN__
            except IndexError:
                isprerun = False

            if line != "\n" and not line.startswith("-->") and line.strip() != '' and not isprerun:
                line = line.split("-->")[0].strip()

                should_run = True

                if line.startswith("RETURN"):
                    funcReturn, succ = __RUN__(line, cline+1, __LOCALS__, True, False)
                    break

                if insideFunc > 0 and not shouldRunFuncCode:
                    should_run = line.startswith("FUNCTION") or line.startswith("END")


                if should_run:
                    status, succ = __RUN__(line, cline+1, __LOCALS__, True, False, cline == func.line)

        except KeyboardInterrupt:
            __DEBUG_INFO__(level="INFO", write_to_file=False, close_program=True, message="KEYBOARD INTERRUPTION", line=cline, exit_code=0)
        finally:
            if status == "leave":
                break
            elif str(status).startswith("skip_to:"):
                status = status[8:]
                try:
                    tempcline = locals["__LABELS__"][status]
                    if tempcline < func.line:
                        return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT LEAVE FUNCTION THROUGH JUMP LABEL", cline)
                    if __DEVMODE__:
                        print(f"SKIPPING TO LABEL {status} (LINE: {locals['__LABELS__'][status]})")
                    cline = tempcline
                    continue
                except KeyError:
                    __DEBUG_INFO__(level="WARN", write_to_file=True, message=f"LABEL {status} IS NOT DEFINED IN THE CURRENT SCOPE, CONTINUING PROGRAM", line=cline+1)
            elif str(status).startswith("line:"):
                break


            cline+=1

    return [funcReturn, True]

def interpret():
    global __GLOBALS__, insideFunc, shouldRunFuncCode, __TO_PRERUN__
    for __FILE__ in __GLOBALS__["__FILES__"]:
        __GLOBALS__["__PROGRAM_FILEPATH__"] = __FILE__
        __GLOBALS__["__FILE_LINES__"] = []
        __GLOBALS__["__FILE_LINES__"], succ = __OPEN_FILE__(__FILE__)
        lines = __GLOBALS__["__FILE_LINES__"]
        verfile = __FILE__+":version"
        filever = None
        status = True

        __PRE_RUN__("#DEFINE", lines, __GLOBALS__)

        try:
            with open(verfile, "r") as f:
                filever = f.read().strip()
        except FileNotFoundError:
            if not __GLOBALS__["__IGNORE_VERSION_WARNS__"]:
                print(f"[WARN] THIS FILE DOES NOT CONTAIN A VERSION HEADER, THEREFORE IT MAY NOT RUN CORRECTLY ON THE CURRENT VERSION ({__CURRENT_VERSION__})")
                print(f"[INFO] SIGNING WITH RECENT VERSION TO SILENCE THE WARNING...")
                with open(verfile, "w") as f:
                    f.write(str(__CURRENT_VERSION__))
                time.sleep(2)

        if filever and not __GLOBALS__["__IGNORE_VERSION_WARNS__"]:
            parsedfilever = parse(filever)
            parsedcrrver = __CURRENT_VERSION__
            if parsedfilever < parsedcrrver:
                print(f"[WARN] THIS FILE CONTAINS A OLDER VERSION HEADER, THEREFORE IT MAY NOT RUN CORRECTLY ON THE CURRENT VERSION (CURRENT: {__CURRENT_VERSION__} || FILE: {filever})")
                print(f"[INFO] SIGNING WITH RECENT VERSION TO SILENCE THE WARNING...")
                with open(verfile, "w") as f:
                    f.write(str(__CURRENT_VERSION__))
                time.sleep(2)
            elif parsedfilever > parsedcrrver:
                print(f"[WARN] THIS FILE CONTAINS A NEWER VERSION HEADER, THEREFORE IT MAY NOT RUN CORRECTLY ON THE CURRENT VERSION (CURRENT: {__CURRENT_VERSION__} || FILE: {filever})")
                time.sleep(2)
        if __DEVMODE__:
            print("-----------\n", __GLOBALS__["__FILE_LINES__"], "\n\n\n-----------")
        cline = 0

        for command in __TO_PRERUN__:
            if command != "#DEFINE":
                __PRE_RUN__(command, lines, __GLOBALS__)


        while cline < len(lines):
            try:
                line:str = lines[cline]
                status = ""

                isprerun = line.strip() in __TO_PRERUN__

                if line != "\n" and not line.startswith("-->") and line.strip() != '' and not isprerun:
                    line = line.split("-->")[0].strip()

                    should_run = True

                    if insideFunc > 0 and not shouldRunFuncCode:
                        should_run = line.startswith("FUNCTION") or line.startswith("END")


                    if should_run:
                        status, succ = __RUN__(line, cline+1, __GLOBALS__)

            except KeyboardInterrupt:
                __DEBUG_INFO__(level="INFO", write_to_file=False, close_program=True, message="KEYBOARD INTERRUPTION", line=cline, exit_code=0)
            finally:
                if status == "leave":
                    break
                elif str(status).startswith("skip_to:"):
                    status = status[8:]
                    try:
                        cline = __GLOBALS__["__LABELS__"][status]
                        if __DEVMODE__:
                            print(f"SKIPPING TO LABEL {status} (LINE: {__GLOBALS__['__LABELS__'][status]})")
                        continue
                    except KeyError:
                        __DEBUG_INFO__(level="WARN", write_to_file=True, message=f"LABEL {status} IS NOT DEFINED IN THE CURRENT SCOPE, CONTINUING PROGRAM", line=cline+1)
                elif str(status).startswith("function:"):
                    funcname = status[9:]
                    cline = (__GLOBALS__["__FUNCTIONS__"][funcname].line)
                    shouldRunFuncCode = True
                    continue
                elif str(status).startswith("line:"):
                    line = status[5:]
                    line, succ = __CONVERT_TO__(line, "int", cline, True, locals)
                    if not succ:
                        __DEBUG_INFO__("CRITICAL", True, True, "INNER ERROR WHILE TRYING TO SKIP LINE", cline)
                        break
                    else:
                        cline = line
                        continue


                cline+=1

        if __GLOBALS__["__DEBUG__"]:
            print("PROGRAM LEFT WITH EXIT CODE: 0")
        if __DEVMODE__:
            print("\n\n\n-----------\n", __GLOBALS__["__FILE_LINES__"])

def console():
    global __CONSOLE_MODE__
    unsupported = ["LABEL", "#DEFINE", "JUMP"]
    now = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    plat = "Unknown"
    platform = __GLOBALS__["__PLATFORM__"]
    if platform.startswith("win") or platform.startswith("cygwin") or platform.startswith("msys"):
        plat = "Windows"
    elif platform.startswith("darwin"):
        plat = "MacOS"
    elif platform.startswith("linux"):
        plat = "Linux"
    elif platform.startswith("os"):
        plat = "OS"
    elif platform.startswith("riscos"):
        plat = "RiscOS"
    elif platform.startswith("atheos"):
        plat = "AtheOS"
    elif platform.startswith("freebsd"):
        plat = f"FreeBSD"
    elif platform.startswith("openbsd"):
        plat = f"OpenBSD"
    elif platform.startswith("aix"):
        plat = "AIX"

    print(f"MowLang {__CURRENT_VERSION__} ({now}) [platform: {plat} || sys: {platform} || type: {__GLOBALS__['__OS_NAME__']}]")
    if __GLOBALS__["__DEBUG__"]:
        print("Debug mode: ON")
    print("Type 'EXIT' to quit.\n")
    try:
        keyword = input("~> ")
        while keyword.strip() != "EXIT":
            __GLOBALS__["__FILE_LINES__"].append(keyword)
            if keyword.split()[0] in unsupported:
                print(f"> [INFO] UNFORTUNATELY, THE CONSOLE MODE CANNOT EXECUTE {keyword.split()[0]} OPERATIONS")
            elif keyword.endswith(".mow"):
                tmplines, succ = __OPEN_FILE__(keyword)
                if succ:
                    print(f"> [INFO] RUNNING FILE {keyword}")
                    __CONSOLE_MODE__ = False
                    __GLOBALS__["__FILES__"].append(keyword)
                    __GLOBALS__["__FILE_LINES__"] = []
                    interpret()
                    __CONSOLE_MODE__ = True
            else:
                status, succ = __RUN__(keyword, 1, __GLOBALS__)
                if status == "leave":
                    break

            keyword = input("~> ")

        if __GLOBALS__["__DEBUG__"]:
            print("LEAVING WITH EXIT CODE: 0")

    except KeyboardInterrupt:
        print("\n> [INFO] LEAVING...")

if len(__GLOBALS__["__FILES__"]) > 0:
    interpret()
else:
    console()