import sys, os, time, datetime, codecs, math, random
from packaging.version import parse, Version, InvalidVersion

################################################################
# TODO: 1. DOCUMENTAR A LINGUAGEM COM SITE                     #
#                                                              #
# DONE: 1. NADA                                                #
################################################################




global currentFile

__DEVMODE__ = 1 == 0
__CONSOLE_MODE__ = False

try:
    with open("VERSION", "r") as f:
        data = f.readlines()
        v = Version(data[0].strip())
        b = int(data[1].strip())
        d = data[2].strip()
except FileNotFoundError:
    v = Version("2.0.0")
    b = 105
    d = "2025-08-31"

__CURRENT_VERSION__ = v
__BUILD__ = b
__BUILD_DATE__ = d
__RUNTIME__ = datetime.datetime.now()
__TIMER__ = None
__TO_PRERUN__ = ["#DEFINE", "LABEL"]
insideFunc = 0
recentOpenFuncs = []
lineBeforeCallingFunc = -1
shouldRunFuncCode = False
data_types = ["str", "int", "float", "bool", "char", "list", "dict", "null"]
currentFile = "local"
shouldSkipCode = [False]
indent = 0

__GLOBALS__ = {
    "__NAME__": __name__,
    "__PROGRAM_FILEPATH__": "",
    "__DEBUG__": False,
    "__FILES__": [],
    "__LABELS__": {},
    "__VARIABLES__": {
        "local": {}
    },
    "__FUNCTIONS__": {
        "local": {}
    },
    "__FILE_LINES__": {
        "local": []
    },
    "__STACK__": None,
    "__IGNORE_WARNS__": False,
    "__DEFAULT_READ_TEXT__": "",
    "__MIN_VER__": Version("0.0.1"),
    "__OS_NAME__": os.name,
    "__PLATFORM__": sys.platform
}

class Var:
    """
        CREATES VARIABLES RECIEVING JUST THEIR NAME FOR REFERENCE, TYPE AND VALUE
    """

    def __init__(self, ref_name:str, typeof:str, value:any):
        typeof = typeof.lower()
        ft = typeof
        self.ref_name = ref_name
        self.typeof = typeof
        self.value = value
        if typeof == "int":
            ft = "integer"
        elif typeof == "str":
            ft = "string"
        elif typeof == "char":
            ft = "character"
        elif typeof == "dict":
            ft = "dictionary"
        elif typeof == "bool":
            ft = "boolean"

        self.fulltype = ft

class Stack:
    """
        CREATES STACKS AND THEIR ATTRIBUTES
    """

    def __init__(self, size, cline):
        if size < 1:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT ALLOCATE STACK WITH SIZE OF {size}", line=cline)
        if __GLOBALS__["__DEBUG__"]:
            print(f"ALLOCATING STACK WITH SIZE OF {size}")
        self.buffer = [0 for _ in range(size)]
        self.pointer = -1
        self.size = size

        if __GLOBALS__["__STACK__"]:
            old:Stack = __GLOBALS__["__STACK__"]
            if len(old.buffer) < size:
                for i, val in enumerate(old.buffer):
                    self.buffer[i] = val
                self.pointer = old.pointer
            else:
                for i, val in enumerate(self.buffer):
                    self.buffer[i] = old.buffer[i]
                    self.pointer = i


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
            self.buffer[self.pointer] = 0
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
        return True

    def swap(self, cline):
        try:
            self.buffer[self.pointer-1], self.buffer[self.pointer] = self.buffer[self.pointer], self.buffer[self.pointer-1]
            return self.buffer[self.pointer]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="INDEX OUT OF RANGE FOR TYPE STACK", line=cline)
        except Exception as e:
            return __DEBUG_INFO__("CRITICAL", True, True, f"{str(e.with_traceback).upper()}\n\t{str(e).upper()}", cline)
        
    def dup(self, cline):
        try:
            self.buffer[self.pointer+1] = self.buffer[self.pointer]
            self.pointer+=1
            return self.buffer[self.pointer]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="INDEX OUT OF RANGE FOR TYPE STACK", line=cline)
        except Exception as e:
            return __DEBUG_INFO__("CRITICAL", True, True, f"{str(e.with_traceback).upper()}\n\t{str(e).upper()}", cline)

    def rot(self, cline):
        try:
            self.buffer =  self.buffer[::-1]
            return self.buffer
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="INDEX OUT OF RANGE FOR TYPE STACK", line=cline)
        except Exception as e:
            return __DEBUG_INFO__("CRITICAL", True, True, f"{str(e).upper()}\n\t{str(e).upper()}", cline)

class Func:
    """
        CREATES FUNCTIONS
    """
    def __init__(self, name:str, refline:int, args:list, file:str):
        self.args={}
        self.name = name
        self.line = refline
        self.file = file
        for i, arg in enumerate(args):
            self.args[f"arg{i}"] = arg

    def __str__(self):
        args = []
        for i, v in self.args.items():
            args.append(v)
        return f"|{self.file}: {self.line+1}| {self.name}({', '.join(args)})"

def __DEBUG_INFO__(level="WARN", write_to_file=False, close_program=False, message="Verify language developer, no argument provided to logging this error", line=-1, exit_code=-1, locals=__GLOBALS__):
    """
        DEBUG TRACEBACK, PRINTS ERRORS AND STUFF
    """
    global currentFile
    if level == "INFO":
        writable = f"[{level}] {message}"
    else:
        writable = f"TRACING RECENT CALL:\n\t[{level}] {message}\n\tKEYWORD: {__OPEN_FILE__(currentFile)[0][line].strip()}\n\tAT LINE: {line}\n\tFILE: {currentFile if currentFile == 'local' else currentFile+'.mow'}"
        try:
            writable = f"TRACING RECENT CALL:\n\t[{level}] {message}\n\tKEYWORD: {locals['__FILE_LINES__'][currentFile][line-1].strip()}\n\tAT LINE: {line}\n\tFILE: {currentFile if currentFile == 'local' else currentFile+'.mow'}\n" if not __CONSOLE_MODE__ else f"TRACING RECENT CALL:\n\t[{level}] {message}\n\tKEYWORD: {locals['__FILE_LINES__']['local'][-1]}\n\tAT LINE: 1\n"
        except KeyError:
            f:list = __GLOBALS__["__FILES__"]
            if len(f) > 1:
                writable = f"TRACING RECENT CALL:\n\t[CRITICAL] TRIED TO CALL INTERPRETER WITH NON-EXISTING FILES\n\tKEYWORD: [{', '.join(f)}]\n\tFILE: {currentFile if currentFile == 'local' else currentFile+'.mow'}" +"\n\t\tSUBMESSAGE: "+ message
            else:
                writable = f"TRACING RECENT CALL:\n\t[CRITICAL] TRIED TO CALL INTERPRETER WITH NON-EXISTING FILE\n\tKEYWORD: [{f[0]}]\n\tFILE: {currentFile if currentFile == 'local' else currentFile+'.mow'}" +"\n\t\tSUBMESSAGE: " + message
                __GLOBALS__["__FILES__"].pop()
            print("\n\n" + writable)
            print("\nPROGRAM LEFT WITH EXIT CODE: -1")
            quit()
        if write_to_file and not __CONSOLE_MODE__:
            with open(locals["__PROGRAM_FILEPATH__"].split("/")[-1] + ".log", "a") as f:
                f.write(writable + "\n")
    if not __GLOBALS__["__IGNORE_WARNS__"] and level == "INFO":
        print("\n"+writable+"\n")
    else:
        print("\n\n"+writable)
    if close_program and level in ["CRITICAL", "ERROR"]:
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
    """
        FORMATS STRINGS IF THEY START AND END IN DOUBLE/SINGLE QUOTES
    """
    try:
        parts = literal.split(None, 0+offset)
    except AttributeError:
        return [literal, False]
    if len(parts) <= 0+offset:
        return [literal, False]
    lit = parts[0+offset].rstrip("\n")
    lit = codecs.decode(lit, "unicode_escape")
    is_string = (lit.startswith('"') and lit.endswith('"')) or (lit.startswith("'") and lit.endswith("'"))
    return [lit, is_string]

def __DICT_LIST_PARSER__(value:str, typeof:str, cline:int, locals:dict, error:bool=True):
    """
        PARSES CORRECTLY FORMATTED LIST/DICT INTO RESPECTIVE VALID VALUES NOT STRING
    """

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

            keyliteral, keyis_string = __IS_STRING__(keypart)
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
        return parse_dict(value, locals)
    else:
        return [parse_container(value, locals), True]

def __CONVERT_TO__(value, target_type, cline=-1, error:bool=True, locals:dict={}):
    """
        CONVERTS STRING VALUE TO ANY VALUE GIVEN IF IT IS FORMATTED CORRECTLY FOR THAT VALUE\n
        (e.g.: "{"dict": "valid dict value"}" <-- IF TRIED TO CONVERT THIS STRING, IT WOULD RETURN A VALID DICT\n
        "[1, 2, 3, 4]" <-- VALID LIST, \"\"[1, 2, 3, 4]\"\" <-- INVALID LIST, IT WOULD DETECT A STRING, NOT LIST)
    """
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

    if target_type == "null":
        return [None, True]

    if target_type == "str":
        literal, is_string = __IS_STRING__(value)
        lit2, s = __RUN__(value, cline, locals, False)
        if is_string:
            literal = literal[1:-1]
        elif s and type(lit2) not in [list, dict]:
            return [str(lit2), True]
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
        literal, is_string = __IS_STRING__(value)
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
        return [val, succ]
    elif target_type == "*":
        val, succ, typeof = parse_num(value)
        if succ:
            return [val, succ]
        val, succ = parse_list(value, False)
        if succ:
            return [val, succ]
        val, succ = parse_dict(value, False)
        if succ:
            return val
        val, succ = __IS_STRING__(val)
        if succ:
            return [val[1:-1], succ]
        else:
            return __RUN__(val, cline, locals, False)

    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"TYPE {target_type} NOT SUPPORTED OR VALUE INVALID: {value}", line=cline)

def __OPEN_FILE__(filepath):
    """
        PRETTY MUCH SELF-EXPLANATORY, BUT IT JUST OPENS FILES AND STORES THEM INSIDE \_\_GLOBALS\_\_
    """
    try:
        return [__GLOBALS__["__FILE_LINES__"][filepath], True]
    except KeyError:
        try:
            with open(filepath, "r") as f:
                data = f.readlines()
            __GLOBALS__["__FILE_LINES__"][filepath] = data
            return [data, True]
        except FileNotFoundError:
            try:
                with open(filepath+".mow", "r") as f:
                    data = f.readlines()
                __GLOBALS__["__FILE_LINES__"][filepath] = data
                return [data, True]
            except FileNotFoundError:
                return __DEBUG_INFO__("CRITICAL", True, True, f"CANNOT FIND PATH {filepath}, VERIFY IF THERE WASN'T ANY TYPOS")

def __PARSE_FUNC__(maybefunc: str, warn: bool = True, cline: int = -1, locals:dict={}, scope:str="local"):
    """
        RECIEVES STRING VALUE ENDED IN () (e.g.: "MyFunc("Argument 1", 2, ["Argument", 3], {"Argument": 4})"), PARSES IT AND IF IT FINDS INSIDE THE DICT locals FROM scope, IT RETURNS THE FUNCTION AND ITS ARGUMENTS
    """
    s = maybefunc.strip()

    pos = s.find("(")
    funcname = s[:pos].strip() if pos != -1 else False
    #print(maybefunc,"\n", scope)
    if not funcname:
        if warn:
            #print("cannot find func name")
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT FIND FUNCTION NAME", cline)
        return [False, False]

    try:
        func = locals["__FUNCTIONS__"][scope][funcname]
        #print(str(func))
    except KeyError:
        if warn:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT FIND FUNCTION NAMED {funcname}. FUNCTION NOT DECLARED ON THE SCOPE {scope}", cline)
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
            elif c == ";" and brace_depth == 0 and bracket_depth == 0 and paren_depth == 0:
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

def __CREATE_LOCALS__(predef:dict|bool=False) -> dict:
    """
        SIMPLY GENERATES A NEW LOCAL BASED ON ANOTHER LOCAL OR A COMPLETELY NEW LOCAL, USEFUL FOR OPENING DIFFERENT MODULES
    """
    if predef:
        locals = {
            "__NAME__": predef["__NAME__"],
            "__PROGRAM_FILEPATH__": "",
            "__VARIABLES__": predef["__VARIABLES__"],
            "__FILES__": [],
            "__DEBUG__": predef["__DEBUG__"],
            "__LABELS__": {},
            "__FUNCTIONS__": predef["__FUNCTIONS__"],
            "__FILE_LINES__": {
                "local": []
            },
            "__STACK__": None,
            "__DEFAULT_READ_TEXT__": "",
            "__PLATFORM__": predef["__PLATFORM__"],
            "__IGNORE_WARNS__": predef["__IGNORE_WARNS__"],
            "__MIN_VER__": predef["__MIN_VER__"],
            "__OS_NAME__": predef["__OS_NAME__"]
        }
    else:
        locals = {
            "__NAME__": __name__,
            "__PROGRAM_FILEPATH__": "",
            "__DEBUG__": False,
            "__FILES__": [],
            "__LABELS__": {},
            "__VARIABLES__": {},
            "__FUNCTIONS__": {},
            "__FILE_LINES__": {
                "local": []
            },
            "__STACK__": None,
            "__IGNORE_WARNS__": False,
            "__DEFAULT_READ_TEXT__": "",
            "__MIN_VER__": Version("0.0.1"),
            "__OS_NAME__": os.name,
            "__PLATFORM__": sys.platform
        }
    
    return locals

def __SEARCH_DICT_LIST__(variable:str, locals:dict, scope:str="local", cline:int=-1):
    """
        RECIEVES STRING VALUE (e.g.: "MyListVar[1]"), DICT locals (e.g.: \_\_GLOBALS\_\_),\nSTRING scope (e.g.: "local", "mowFile", "example1"), INTEGER cline (e.g.: 1, 8, 52)
    """
    index = "".join(("".join(variable.split("[")[1:])).split("]")[:-1])
    varname = variable.split("[")[0].strip()

    try:
        var:Var = locals["__VARIABLES__"][scope][varname]
    except KeyError:
        return __DEBUG_INFO__("ERROR", True, True, f"VARIABLE {varname} IS NOT DEFINED INSIDE THE CURRENT SCOPE {scope}", cline, -1, locals)

    rval = var.value

    if var.typeof in ["float", "int", "bool", "null"]:
        return __DEBUG_INFO__("CRITICAL", True, True, f"{var.fulltype.upper()} TYPE IS NOT ITERABLE", cline, -1, locals)
    elif var.typeof == "char":
        if not locals["__IGNORE_WARNS__"]:
            __DEBUG_INFO__(write_to_file=True, close_program=False, message=f"{var.fulltype.upper()} TYPE VARIABLE MAY CAUSE INDEX ERRORS MORE OFTEN", line=cline, locals=locals)

    if not ':' in index:
        if var.typeof in ["list", "str", "char"]:
            index, s = __CONVERT_TO__(index, "float", cline, False, locals)
            if not s:
                index, s = __RUN__(index, cline, locals, False)
            if type(index) not in [int, float]:
                return __DEBUG_INFO__("CRITICAL", True, True, "TO FIND VALUE IN LIST TYPE, USE INTEGER TYPE VALUE", cline, -1, locals)
            index = int(index)
        elif var.typeof == "dict":
            index, s = __IS_STRING__(index)
            if not s:
                index, s = __RUN__(index, cline, locals, False)
                if not s:
                    return __DEBUG_INFO__("CRITICAL", True, True, f"KEY \"{index}\" MUST BE STRING", cline, -1, locals)
            else:
                index = index[1:-1]

        try:
            rval = var.value[index]
        except KeyError:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT FIND KEY \"{index}\"", cline, -1, locals)
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, f"{var.typeof.upper()} INDEX ({index}) OUT OF RANGE", cline, -1, locals)
        except Exception as e:
            return __DEBUG_INFO__("CRITICAL", True, True, str(e).upper(), cline, -1, locals)
    else:
        v = index.strip().split(":")
        if len(v) == 2 and v[0] != '' and v[1] != '':
            v1, s = __CONVERT_TO__(v[0], "int", cline, True, locals)
            v2, s = __CONVERT_TO__(v[1], "int", cline, True, locals)
            rval = var.value[v1:v2]
        elif len(v) == 1 or (v[0] == '' and v[1] != '') or (v[0] != '' and v[1] == ''):
            if variable.split(":")[0][-1].strip() == "[":
                v1 = 0
                v2, s = __CONVERT_TO__(v[1], "int", cline, True, locals)
                rval = var.value[:v2]
            else:
                v1, s = __CONVERT_TO__(v[0], "int", cline, True, locals)
                v2 = 0
                rval = var.value[v1:]
        else:
            return __DEBUG_INFO__("ERROR", True, True, f"INVALID EXPRESSION {variable}")

    return [rval, True]

def __RUN__(__LINE__:str, cline:int, locals:dict, warn: bool = True, *kargs):
    """
        RUNS COMMANDS\n
        NO SERIOUSLY, JUST THAT\n
        RETURNS ["leave", False] IF INVALID SYNTAX IS PROVIDED
    """
    # SETTING THINGS UP
    global __GLOBALS__, insideFunc, recentOpenFuncs, lineBeforeCallingFunc, shouldRunFuncCode, pid, currentFile, shouldSkipCode

    #with open("console.log", "a") as f:
    #    f.write(f"\nRunning {__LINE__.strip()} in {currentFile}")
    __LINE__ = __LINE__.split("-->")[0].strip()

    if __LINE__ == "":
        return [True, True]
    
    c = __LINE__.split()[0].split(".")[0].split("(")[0].strip()
    try:
        method = __LINE__.split()[0].split('.')[1].strip()
    except IndexError:
        method = None
    args = __LINE__.split()[1:]

    # ACTUALLY RUNNING FILE CODE
    # PRINTS TO CONSOLE
    if c in ["PRINT", "PRINTL"]:
        nextline = c[-1] == "L"

        if len(args) == 0 and not method:
            return __DEBUG_INFO__("ERROR", True, True, "CANNOT PRINT NOTHING", cline)
        literal, is_string = __IS_STRING__(" ".join(args))
        val = None

        if method == "VARS":
            prints = []
            for local in locals["__VARIABLES__"]:
                for var in locals["__VARIABLES__"][local]:
                    ref_name = locals["__VARIABLES__"][local][var].ref_name
                    typeof = locals["__VARIABLES__"][local][var].typeof
                    value = locals["__VARIABLES__"][local][var].value
                    prints.append(f"[{local.upper()}] |{typeof.upper()}| {ref_name}: {value}")
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
    elif c == "STACK":
        if not method:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="PROVIDE METHOD WHEN USING STACK COMMAND", line=cline)

        if method == "ALLOC":
            try:
                arg = args[0].strip()
                if arg == '':
                    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO PROVIDE STACK SIZE ARGUMENT TO ALLOCATE MEMORY", line=cline)
                size = int(arg)
                locals["__STACK__"] = Stack(size, cline)
                return [True, True]
            except IndexError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO PROVIDE STACK SIZE ARGUMENT TO ALLOCATE MEMORY", line=cline)
            except ValueError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE THAT ARGUMENT PROVIDED TO ALLOCATE MEMORY IS A POSITIVE INTEGER TYPE", line=cline)
            except Exception as e:
                return __DEBUG_INFO__(level="CRITICAL", write_to_file=True, close_program=True, message=f"ERROR NOT TREATED CORRECTLY, CONTACT LANGUAGE DEVELOPER FOR MORE INFO ALONG WITH THE ERROR:\n\t{str(e.with_traceback)}\n\t{str(e)}", line=cline)
        elif method == "PUSH":
            datatype, raw_value = args[0], " ".join(args[1:])
            datatype = datatype.lower()

            if datatype not in data_types:
                return __DEBUG_INFO__("ERROR", True, True, f"DATA TYPE {datatype} IS NOT SUPPORTED", cline, -1, locals)

            if datatype != "null":
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
            else:
                raw_value = None

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
        elif method == "ROT":
            try:
                return [locals["__STACK__"].rot(cline), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
        elif method == "DUP":
            try:
                return [locals["__STACK__"].dup(cline), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)
        elif method == "SWAP":
            try:
                return [locals["__STACK__"].swap(cline), True]
            except AttributeError:
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MAKE SURE TO ALLOCATE STACK MEMORY BEFORE TRYING ANY STACK OPERATION", line=cline)


        elif len(method) == 0:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="PROVIDE METHOD WHEN USING STACK COMMAND", line=cline)
        else:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"STACK METHOD {method} IS NOT DEFINED IN THE CURRENT SCOPE", line=cline)

    # MANAGE LABEL OPERATIONS
    elif c == "LABEL":
        for l in args:
            locals["__LABELS__"][l.strip()] = cline
        return [cline, True]

    # MANAGE STRING INPUT OPERATIONS
    elif c == "READ":
        try:
            datatype = args[0]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "CANNOT READ none TYPE", cline, -1, locals)

        if len(args) > 1:
            text = " ".join(args[1:])
            text, s = __IS_STRING__(text)
            if not s:
                text, s = __RUN__(text, cline, locals, False)
            else:
                text = text[1:-1]
            
            if not s:
                return __DEBUG_INFO__("ERROR", True, True, "READ \"TEXT\" ARGUMENT MUST BE STRING OR RUNNABLE COMMAND THAT NOT RETURNS NONE", cline, -1, locals)
        else:
            text = locals["__DEFAULT_READ_TEXT__"]

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
    elif c in ["EQ", "=="]:
        args = "".join(args).strip().split(",")
        if len(args) != 2:
            return __DEBUG_INFO__("ERROR", True, True, "INVALID NUMBER OF PARAMETERS FOR COMPARISON OPERATION, 2 REQUIRED", cline, -1, locals)
        
        cmp = args[1].strip()

        result, succ = __RUN__(cmp, cline, locals, False)
        if succ:
            cmp = result

        top = args[0].strip()
        result, succ = __RUN__(top, cline, locals, False)
        if succ:
            top = result

        try:
            comp2 = float(cmp)
            comp = float(top)
        except ValueError:
            comp, t = __CONVERT_TO__(top, "*", cline, False, locals)
            comp2, t =__CONVERT_TO__(cmp, "*", cline, False, locals)

        return [comp == comp2, True]

    elif c in ["GTE", ">="]:
        args = "".join(args).strip().split(",")
        if len(args) != 2:
            return __DEBUG_INFO__("ERROR", True, True, "INVALID NUMBER OF PARAMETERS FOR COMPARISON OPERATION, 2 REQUIRED", cline, -1, locals)

        cmp = args[1].strip()

        result, succ = __RUN__(cmp, cline, locals, False)
        if succ:
            cmp = result

        top = args[0].strip()
        result, succ = __RUN__(top, cline, locals, False)
        if succ:
            top = result

        try:
            comp2 = float(cmp)
            comp = float(top)
        except ValueError:
            comp, t = __CONVERT_TO__(top, "str", cline, True, locals)
            comp2, t = __CONVERT_TO__(cmp, "str", cline, True, locals)
            comp, t = __CONVERT_TO__(comp, "float", cline, True, locals)
            comp2 = t = __CONVERT_TO__(comp2, "float", cline, True, locals)

        try:
            return [comp >= comp2, True]
        except Exception as e:
            return [False, True]

    elif c in ["LTE", "<="]:
        args = "".join(args).strip().split(",")
        if len(args) != 2:
            return __DEBUG_INFO__("ERROR", True, True, "INVALID NUMBER OF PARAMETERS FOR COMPARISON OPERATION, 2 REQUIRED", cline, -1, locals)

        cmp = args[1].strip()

        result, succ = __RUN__(cmp, cline, locals, False)
        if succ:
            cmp = result

        top = args[0].strip()
        result, succ = __RUN__(top, cline, locals, False)
        if succ:
            top = result

        try:
            comp2 = float(cmp)
            comp = float(top)
        except ValueError:
            comp, t = __CONVERT_TO__(top, "str", cline, True, locals)
            comp2, t =__CONVERT_TO__(cmp, "str", cline, True, locals)
            comp, t = __CONVERT_TO__(comp, "float", cline, True, locals)
            comp2 = t = __CONVERT_TO__(comp2, "float", cline, True, locals)

        try:
            return [comp <= comp2, True]
        except Exception as e:
            return [False, True]

    elif c in ["GT", ">"]:
        args = "".join(args).strip().split(",")
        if len(args) != 2:
            return __DEBUG_INFO__("ERROR", True, True, "INVALID NUMBER OF PARAMETERS FOR COMPARISON OPERATION, 2 REQUIRED", cline, -1, locals)

        cmp = args[1].strip()

        result, succ = __RUN__(cmp, cline, locals, False)
        if succ:
            cmp = result

        top = args[0].strip()
        result, succ = __RUN__(top, cline, locals, False)
        if succ:
            top = result

        try:
            comp2 = float(cmp)
            comp = float(top)
        except ValueError:
            comp, t = __CONVERT_TO__(top, "str", cline, True, locals)
            comp2, t = __CONVERT_TO__(cmp, "str", cline, True, locals)
            comp, t = __CONVERT_TO__(comp, "float", cline, True, locals)
            comp2, t = __CONVERT_TO__(comp2, "float", cline, True, locals)

        try:
            return [comp > comp2, True]
        except Exception as e:
            return [False, True]

    elif c in ["LT", "<"]:
        args = "".join(args).strip().split(",")
        if len(args) != 2:
            return __DEBUG_INFO__("ERROR", True, True, "INVALID NUMBER OF PARAMETERS FOR COMPARISON OPERATION, 2 REQUIRED", cline, -1, locals)

        cmp = args[1].strip()

        result, succ = __RUN__(cmp, cline, locals, False)
        if succ:
            cmp = result

        top = args[0].strip()
        result, succ = __RUN__(top, cline, locals, False)
        if succ:
            top = result

        try:
            comp2 = float(cmp)
            comp = float(top)
        except ValueError:
            comp, t = __CONVERT_TO__(top, "str", cline, True, locals)
            comp2, t = __CONVERT_TO__(cmp, "str", cline, True, locals)
            comp, t = __CONVERT_TO__(comp, "float", cline, True, locals)
            comp2 = t = __CONVERT_TO__(comp2, "float", cline, True, locals)

        try:
            return [comp < comp2, True]
        except Exception as e:
            return [False, True]

    # MANAGE JUMP OPERATIONS
    elif c == "JUMP":
        if len(args) > 3:
            try:
                method, cmp, label = args[0], args[1] + " " + args[2], args[3]
                templine = method + " " + cmp
                result, succ = __RUN__(templine, cline, locals)
                if result:
                    return ["skip_to:"+label, True]
            except IndexError:
                return __DEBUG_INFO__(level="ERROR", close_program=True, write_to_file=True, message=f"MAKE SURE THE JUMP LABEL IS RECIEVING COMPARISON METHOD, COMPARISON VALUE AND STRING LABEL NAME THAT IS NOT EMPTY", line=cline)
        elif len(args) == 1 and args[0] != '':
            label = args[0]
            return ["skip_to:"+label, True]
        else:
            return __DEBUG_INFO__(level="ERROR", close_program=True, write_to_file=True, message=f"MAKE SURE THE JUMP LABEL IS RECIEVING AT LEAST THE LABEL NAME ARGUMENT THAT IS NOT EMPTY", line=cline)

        return [True, True]

    # SEQUENCE OF ARITHMETIC OPERATORS
    elif c == "ADD":
        try:
            args = "".join(args).split(",")
            arg1, arg2 = args[0], args[1]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="ADD KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        arg1, s = __IS_STRING__(arg1)
        if not s:
            arg1, s = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
            if not s:
                arg1, s = __RUN__(arg1, cline, locals, False)
        else:
            arg1 = arg1[1:-1]

        literal, is_string = __IS_STRING__(arg2)
        if not is_string:
            arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
            if not succ:
                arg2, succ = __RUN__(arg2, cline, locals, False)
        else:
            arg2 = arg2[1:-1]

        if type(arg1) not in [int, float, str] or type(arg2) not in [int, float, str]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT ADD {str(type(arg2))} + {str(type(arg1))}")

        try:
            arg1 = int(arg1) if int(arg1) == arg1 else arg1
        except ValueError:
            pass

        try:
            arg2 = int(arg2) if int(arg2) == arg2 else arg2
        except ValueError:
            pass

        try:
            result = arg2 + arg1
        except TypeError:
            try:
                result = f"{arg2}" + arg1
            except TypeError:
                result = arg2 + f"{arg1}"

        return [result, True]

    elif c == "SUB":
        try:
            arg1, arg2 = args[0].split(",")[0].strip(), args[1].strip()
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="SUB KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        arg1, s = __IS_STRING__(arg1)
        if not s:
            arg1, s = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
            if not s:
                arg1, s = __RUN__(arg1, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT SUBTRACT FROM STRING", cline)

        literal, is_string = __IS_STRING__(arg2)
        if not is_string:
            arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
            if not succ:
                arg2, succ = __RUN__(arg2, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT SUBTRACT FROM STRING", cline)
        
        t1 = type(arg1)
        t2 = type(arg2)

        if t1 not in [int, float] or t2 not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT SUBTRACT {str(t1)} FROM {str(t2)}")

        arg1 = int(arg1) if int(arg1) == arg1 else arg1
        arg2 = int(arg2) if int(arg2) == arg2 else arg2

        result = arg2 - arg1
        return [result, True]

    elif c == "MUL":
        try:
            args = "".join(args).split(",")
            arg1, arg2 = args[0], args[1]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MUL KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        arg1, s = __IS_STRING__(arg1)
        if not s:
            arg1, s = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
            if not s:
                arg1, s = __RUN__(arg1, cline, locals, False)
        else:
            arg1 = arg1[1:-1]

        literal, is_string = __IS_STRING__(arg2)
        if not is_string:
            arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
            if not succ:
                arg2, succ = __RUN__(arg2, cline, locals, False)
        else:
            arg2 = arg2[1:-1]

        if type(arg1) not in [int, float, str] or type(arg2) not in [int, float, str]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MULTIPLY {str(type(arg2))} BY {str(type(arg1))}")

        try:
            arg1 = int(arg1) if int(arg1) == arg1 else arg1
        except ValueError:
            pass

        try:
            arg2 = int(arg2) if int(arg2) == arg2 else arg2
        except ValueError:
            pass

        t1 = type(arg1)
        t2 = type(arg2)

        if t1 == t2 == str:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MULTIPLY STRING BY STRING", line=cline)
        elif (t1 == str and int(__CONVERT_TO__(arg2, "float", cline, True, locals)[0]) != arg2) or (t2 == str and int(__CONVERT_TO__(arg1, "float", cline, True, locals)[0]) != arg1):
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MULTIPLY STRING BY FLOATING POINT VALUE", line=cline)

        arg1 = int(arg1) if int(arg1) == arg1 else arg1
        arg2 = int(arg2) if int(arg2) == arg2 else arg2

        try:
            result = arg2 * arg1
        except TypeError:
            result = arg1 * arg2

        return [result, True]

    elif c == "DIV":
        try:
            args = "".join(args).split(",")
            arg1, arg2 = args[0], args[1]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="DIV KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        arg1, s = __IS_STRING__(arg1)
        if not s:
            arg1, s = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
            if not s:
                arg1, s = __RUN__(arg1, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT DIVIDE FROM STRING", cline)

        literal, is_string = __IS_STRING__(arg2)
        if not is_string:
            arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
            if not succ:
                arg2, succ = __RUN__(arg2, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT DIVIDE FROM STRING", cline)
        
        t1 = type(arg1)
        t2 = type(arg2)

        if t1 not in [int, float] or t2 not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT DIVIDE {str(t2)} BY {str(t1)}")

        arg1 = int(arg1) if int(arg1) == arg1 else arg1
        arg2 = int(arg2) if int(arg2) == arg2 else arg2

        if arg1 == 0:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT DIVIDE {arg2} BY 0", line=cline)

        result = arg2 / arg1
        return [result, True]

    elif c == "MOD":
        try:
            args = "".join(args).split(",")
            arg1, arg2 = args[0], args[1]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="MOD KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        arg1, s = __IS_STRING__(arg1)
        if not s:
            arg1, s = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
            if not s:
                arg1, s = __RUN__(arg1, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT MODULATE STRING", cline)

        literal, is_string = __IS_STRING__(arg2)
        if not is_string:
            arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
            if not succ:
                arg2, succ = __RUN__(arg2, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT MODULATE STRING", cline)
        
        t1 = type(arg1)
        t2 = type(arg2)

        if t1 not in [int, float] or t2 not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MODULATE {str(t2)} BY {str(t1)}")

        arg1 = int(arg1) if int(arg1) == arg1 else arg1
        arg2 = int(arg2) if int(arg2) == arg2 else arg2

        if arg1 == 0:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT MODULATE BY 0: {arg2} % {arg1}", line=cline)

        result = arg2 % arg1
        return [result, True]

    elif c == "EXP":
        try:
            args = "".join(args).split(",")
            arg1, arg2 = args[0], args[1]
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="EXP KEYWORD NEEDS 2 ARGUMENTS", line=cline)

        arg1, s = __IS_STRING__(arg1)
        if not s:
            arg1, s = __CONVERT_TO__(arg1, "float", cline, False, locals=locals)
            if not s:
                arg1, s = __RUN__(arg1, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT GET THE POWER FROM STRING", cline)

        literal, is_string = __IS_STRING__(arg2)
        if not is_string:
            arg2, succ = __CONVERT_TO__(arg2, "float", cline, False, locals=locals)
            if not succ:
                arg2, succ = __RUN__(arg2, cline, locals, False)
        else:
            return __DEBUG_INFO__("CRITICAL", True, True, "CANNOT GET THE POWER FROM STRING", cline)
        
        t1 = type(arg1)
        t2 = type(arg2)

        if t1 not in [int, float] or t2 not in [int, float]:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"CANNOT ELEVATE {str(t2)} TO THE POWER OF {str(t1)}")

        arg1 = int(arg1) if int(arg1) == arg1 else arg1
        arg2 = int(arg2) if int(arg2) == arg2 else arg2

        result = arg2**arg1
        return [result, True]

    elif c == "NEG":
        try:
            arg = args[0]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "NEG KEYWORD NEEDS VALUE TO NEGATE", cline, -1, locals)

        result, succ = __RUN__(arg, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT NEGATE {str(type(arg))}", cline, -1, locals)
        
        return [-result, True]

    elif c == "INC":
        try:
            arg = args[0]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "INC KEYWORD NEEDS VALUE TO INCREMENT", cline, -1, locals)

        result, succ = __RUN__(arg, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT INCREMENT {str(type(arg))}", cline, -1, locals)
        
        return [result+1, True]
    
    elif c == "DEC":
        try:
            arg = args[0]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "DEC KEYWORD NEEDS VALUE TO DECREMENT", cline, -1, locals)

        result, succ = __RUN__(arg, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT DECREMENT {str(type(arg))}", cline, -1, locals)
        
        return [result-1, True]

    elif c == "SQRT":
        try:
            arg = args[0]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "SQRT KEYWORD NEEDS VALUE TO FIND THE ROOT", cline, -1, locals)

        result, succ = __RUN__(arg, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT FIND THE ROOT OF {str(type(arg))}", cline, -1, locals)
        
        return [math.sqrt(result), True]
    
    elif c == "ABS":
        try:
            arg = args[0]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "ABS KEYWORD NEEDS VALUE TO FIND THE ABSOLUTE", cline, -1, locals)

        result, succ = __RUN__(arg, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"CANNOT FIND THE ABSOLUTE OF {str(type(arg))}", cline, -1, locals)
        
        return [abs(result), True]

    elif c == "MAX":
        try:
            args = "".join(args).split(",")
            arg1, arg2 = args[0], args[1]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "MAX KEYWORD NEEDS 2 NUMBERS IN ORDER TO FIND THE MAXIMUM VALUE")

        result, succ = __RUN__(arg1, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg1, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"TO FIND THE MAXIMUM VALUE, MUST BE INTEGER/FLOAT TYPE. {arg1} IS NOT", cline, -1, locals)
        
        arg1 = result
        
        result, succ = __RUN__(arg2, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg2, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"TO FIND THE MAXIMUM VALUE, MUST BE INTEGER/FLOAT TYPE. {arg1} IS NOT", cline, -1, locals)
        
        arg2 = result

        arg1 = int(arg1) if int(arg1) == arg1 else arg1
        arg2 = int(arg2) if int(arg2) == arg2 else arg2

        result = max(arg1, arg2)
        
        return [result, True]
    
    elif c == "MIN":
        try:
            args = "".join(args).split(",")
            arg1, arg2 = args[0], args[1]
        except IndexError:
            return __DEBUG_INFO__("ERROR", True, True, "MIN KEYWORD NEEDS 2 NUMBERS IN ORDER TO FIND THE MINIMUM VALUE")

        result, succ = __RUN__(arg1, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg1, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"TO FIND THE MINIMUM VALUE, MUST BE INTEGER/FLOAT TYPE. {arg1} IS NOT", cline, -1, locals)
        
        arg1 = result
        
        result, succ = __RUN__(arg2, cline, locals, False)
        if not succ:
            result, succ = __CONVERT_TO__(arg2, "float", cline, False, locals)
        
        if not succ:
            return __DEBUG_INFO__("ERROR", True, True, f"TO FIND THE MINIMUM VALUE, MUST BE INTEGER/FLOAT TYPE. {arg1} IS NOT", cline, -1, locals)
        
        arg2 = result

        arg1 = int(arg1) if int(arg1) == arg1 else arg1
        arg2 = int(arg2) if int(arg2) == arg2 else arg2

        result = min(arg1, arg2)
        
        return [result, True]

    # VARIABLE OPERATORS
    elif c == "STORE":
        try:
            typeof, name = args[0].lower().strip(), args[1].strip()
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="STORE KEYWORD MUST HAVE VARIABLE TYPE AND VARIABLE NAME", line=cline)

        try:
            value = " ".join(args[2:]).strip()
        except IndexError:
            try:
                value = locals["__STACK__"].top()
            except AttributeError:
                value = None

        if typeof not in data_types:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"VARIABLE TYPE MUST BE ONE OF THESE: {', '.join(data_types)}", line=cline)

        if typeof != "null":
            literal, is_string = __IS_STRING__(value)
            if (typeof == "str" or typeof == "char") and not (is_string):
                lit, s = __RUN__(literal, cline, locals, False)
                if not s and warn:
                    return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message="TYPE STRING MUST BE STRING", line=cline)
                elif not s:
                    return [False, False]
                else:
                    literal = lit

            if is_string:
                literal = literal[1:-1]

            if is_string and typeof == "char":
                literal = literal[0]
            elif is_string and typeof in ["int", "float", "bool", "list", "dict"]:
                return __DEBUG_INFO__(level="CRITICAL", write_to_file=True, close_program=True, message=f"TYPE ERROR. TYPE {typeof} CANNOT RECIEVE STRING VALUE", line=cline)

            result, succ = __CONVERT_TO__(f'"{literal}"' if typeof in ["str", "char"] else literal, typeof, cline, False, locals)
            if not succ:
                result, succ = __RUN__(literal, cline, locals, False)

            if not succ:
                return [result, succ]

        value = result if typeof != "null" else None
        var = Var(name, typeof, value)
        locals["__VARIABLES__"][currentFile][name] = var

        return [[var.ref_name, var.typeof, var.value], True]

    elif c == "LOAD":
        try:
            namearg = "".join(args)
        except IndexError:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"LOAD KEYWORD MUST RECIEVE 1 VARIABLE NAME ARGUMENT", line=cline)

        try:
            val = locals["__VARIABLES__"][currentFile][namearg].value
        except KeyError:
            try:
                val = __GLOBALS__["__VARIABLES__"][currentFile][namearg].value
            except KeyError:
                if '[' in namearg and namearg.split("[")[0][-1] != ' ':
                    return __SEARCH_DICT_LIST__(namearg, locals, currentFile, cline)
                return __DEBUG_INFO__(level="ERROR", write_to_file=True, close_program=True, message=f"VARIABLE {namearg} IS NOT DEFINED IN THE CURRENT SCOPE", line=cline)

        return [val, True]

    # PAUSE OPERATOR
    elif c == "PAUSE":
        os.system("pause" if locals["__OS_NAME__"] == "nt" else "read -p \"Press any key to continue . . .\"")
        return [True, True]

    # TIME OPERATOR
    elif c == "TIME":
        global __TIMER__
        if not method:
            return __DEBUG_INFO__("ERROR", True, True, "TIME NEEDS ONE METHOD TO WORK CORRECTLY", cline)

        if method == "SLEEP":
            if len(args) < 1:
                return __DEBUG_INFO__("ERROR", True, True, "TIME.SLEEP MUST RECIEVE ONE FLOAT/INTEGER ARGUMENT", cline)
            else:
                try:
                    secs = float(args[0])
                except ValueError:
                    secs = __CONVERT_TO__(args[0], "float", cline, True, locals)
            time.sleep(secs)
            return [secs, True]
        elif method == "START":
            __TIMER__ = datetime.datetime.now()
            return [str(__TIMER__), True]
        elif method == "STOP":
            if not __TIMER__:
                return __DEBUG_INFO__(write_to_file=True, message="TIMER WAS NOT STARTED BEFORE STOPPED", line=cline)
            t = __TIMER__
            __TIMER__ = None
            return [str(datetime.datetime.now() - t), True]
        elif method == "STAMP":
            return [time.time(), True]
        elif method == "NOW":
            now = " ".join(args[1:]) if len(args) > 2 else "%Y-%m-%d %H:%M:%S"
            local = "LOCAL" in args

            dt = datetime.datetime.now() if local else datetime.datetime.now(datetime.timezone.utc)
            try:
                return [dt.strftime(now), True]
            except ValueError:
                return __DEBUG_INFO__("ERROR", True, True, f"INVALID TIME FORMAT STRING", cline, -1, locals)
        elif method == "SINCE":
            t = datetime.datetime.now() - __RUNTIME__
            return [t.__str__(), True]
        else:
            return __DEBUG_INFO__("ERROR", True, True, f"TIME METHOD {method} IS NOT DEFINED IN THE CURRENT SCOPE", cline, -1, locals)

    # LIBRARY MANAGER
    elif c == "IMPORT":
        if len(args) < 1:
            return __DEBUG_INFO__("ERROR", True, True, "CANNOT RESOLVE NULL IMPORT", cline)

        file = "".join(args)
        literal, is_string = __IS_STRING__(file)
        if is_string:
            file = literal[1:-1]
        
        locals2 = __CREATE_LOCALS__()
        
        try:
            with open(file, "r") as f:
                importlines = f.readlines()
            currentFile = file.strip(".mow")
            for cmd in __TO_PRERUN__:
                __PRE_RUN__(cmd, importlines, locals2)
            __PRE_RUN__("FUNCTION", importlines, locals2, topre=__TO_PRERUN__)
            __PRE_RUN__("EXPORT", importlines, locals2, topre=__TO_PRERUN__)
            currentFile = "local"
        except FileNotFoundError:
            file = file+".mow"
            try:
                with open(file, "r") as f:
                    importlines = f.readlines()
                    currentFile = file.strip(".mow")
                for cmd in __TO_PRERUN__:
                    __PRE_RUN__(cmd, importlines, locals2)
                __PRE_RUN__("FUNCTION", importlines, locals2, topre=__TO_PRERUN__)
                __PRE_RUN__("EXPORT", importlines, locals2, topre=__TO_PRERUN__)
                currentFile = "local"
            except FileNotFoundError:
                return __DEBUG_INFO__("CRITICAL", True, True, f"CANNOT RESOLVE IMPORT FILE {file} FILE MISSING")
        
        return [True, True]

    # INTERPRETER BEHAVIOUR CONFIGURATOR
    elif c == "#DEFINE":
        lar = len(args)

        method = arg2 = arg3 = None

        if lar == 3:
            method, arg2, arg3 = args[0].strip(), args[1].strip(), args[2].strip()
        elif lar == 3:
            method, arg2 = args[0].strip(), args[1].strip()
        elif lar == 2:
            method = args[0].strip()
        else:
            return __DEBUG_INFO__("ERROR", True, True, "#DEFINE COMMAND NEEDS AT LEAST ONE DEFENITION TO DEFINE SOMETHING")

        if method == "NO_WARNS":
            locals["__IGNORE_WARNS__"] = True
            return [True, True]
        elif method == "INPUT_PROMPT":
            if arg3:
                arg2 = arg2 + " " + arg3
            literal, is_string = __IS_STRING__(arg2)
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
    elif c == "FUNCTION":
        insideFunc += 1
        name = args[0]
        params = args[1:]
        func = Func(name, cline, params, currentFile)
        recentOpenFuncs.append(name)

        if not (kargs[0] if len(kargs) > 0 else False):
            if currentFile not in locals["__FUNCTIONS__"]:
                locals["__FUNCTIONS__"][currentFile] = {}
            locals["__FUNCTIONS__"][currentFile][name] = func

        return [[name, cline, params], True]

    # EXPORT SPECIFIC FUNCTIONS/VARIABLES FROM MODULE
    elif c == "EXPORT":
        try:
            tyype = args[0].lower()
            toExpt = " ".join(args[1:]).split(",")
        except IndexError:
            return __DEBUG_INFO__("CRITICAL", True, True,
                f"EXPORT COMMAND MUST RECIEVE THE TYPE (FUNC/VAR) AND THE VARIABLES/FUNCTION NAMES SEPARATED BY COMMAS.")

        toExpt = [f.strip() for f in toExpt if f.strip()]

        if tyype == "func":
            for f in toExpt:
                try:
                    if currentFile not in __GLOBALS__["__FUNCTIONS__"]:
                        __GLOBALS__["__FUNCTIONS__"][currentFile] = {}
                    __GLOBALS__["__FUNCTIONS__"][currentFile][f] = locals["__FUNCTIONS__"][currentFile][f]
                except KeyError:
                    return __DEBUG_INFO__("ERROR", True, True,
                        f"FUNCTION {f} NOT FOUND IN FILE {currentFile} TO EXPORT", cline)
        elif tyype == "var":
            for v in toExpt:
                try:
                    if currentFile not in __GLOBALS__["__VARIABLES__"]:
                        __GLOBALS__["__VARIABLES__"][currentFile] = {}
                    __GLOBALS__["__VARIABLES__"][currentFile][v] = locals["__VARIABLES__"][v]
                except KeyError:
                    return __DEBUG_INFO__("ERROR", True, True,
                        f"VARIABLE {v} NOT FOUND IN FILE {currentFile} TO EXPORT", cline)
        else:
            return __DEBUG_INFO__("ERROR", True, True,
                f"EXPORT TYPE {tyype} NOT SUPPORTED. USE FUNC OR VAR.", cline)

        return [True, True]
    
    # FUNCTION/IF-ELSE TERMINATOR
    elif c == "END":
        name = args[0].strip()
        if recentOpenFuncs[-1] != name:
            return __DEBUG_INFO__("CRITICAL", True, True, f"TRIED TO LEAVE FUNCTION {name} BEFORE CLOSING ITS CHILD FUNCTION", cline)
        insideFunc-=1
        recentOpenFuncs.pop()
        if (insideFunc == 0 or len(recentOpenFuncs) <= 0) and shouldRunFuncCode:
            shouldRunFuncCode = False
            recentOpenFuncs = []
            return [f"line:{lineBeforeCallingFunc}", True]
        elif shouldRunFuncCode:
            return [f"line:{lineBeforeCallingFunc}", True]

        return [[recentOpenFuncs, shouldRunFuncCode, cline, lineBeforeCallingFunc], True]

    elif c == "ENDIF":
        shouldSkipCode.pop()
        return [True, True]

    # FUNCTION RETURNER
    elif c == "RETURN":
        try:
            typeof, value = args[0], " ".join(args[1:])
        except ValueError:
            return [None, True]
        
        lit, is_string = __IS_STRING__(value)
        if not is_string:
            val, succ = __CONVERT_TO__(lit, typeof, cline, False, locals)
        else:
            val = lit[1:-1]
        if not succ:
            val, succ = __RUN__(lit, cline, locals, False)
        return [val, True]

    # IF-ELSE OPERATIONS
    elif c == "IF":
        condition = ")".join(__LINE__[2:].split("(", 1)[1].split(")")[:-1]).strip()

        def parseCondition(condition: str):
            orParams = condition.split("||")
            cond = []
            vers = []

            for param in orParams:
                if "&&" in param:
                    for v in param.split("&&"):
                        cond.append(v.strip())
                else:
                    cond.append(param.strip())

            if len(cond) <= 1:
                return [condition, False]

            for c in cond:
                if c == "":
                    continue
                result, succ = __RUN__(c, cline, locals, False)
                if not succ:
                    return [False, False]
                vers.append(bool(result))

            tmp = condition
            i = 0
            for c in cond:
                if c == "":
                    continue
                tmp = tmp.replace(c, str(vers[i]), 1)
                i += 1

            expr = tmp.replace("&&", " and ").replace("||", " or ")
            try:
                rval = eval(expr)
            except Exception:
                return [False, False]

            return [rval, True]

        result, succ = parseCondition(condition)
        if not succ:
            result, succ = __RUN__(condition, cline, locals, False)
            if not succ:
                return __DEBUG_INFO__("ERROR", True, True, f"INVALID IF CONDITION: {condition}", cline)

        shouldSkipCode.append((not result) if not shouldSkipCode[-1] else True)

        return [result, True]

    elif c == "ELSE":
        shouldSkipCode[-1] = (not shouldSkipCode[-1]) if not shouldSkipCode[-2] else True
        return [shouldSkipCode[-1], True]

    # RETURN __GLOBALS__
    elif c == "GLOBALS":
        return [__GLOBALS__, True]

    # RANDOM NUMBER GENERATOR
    elif c == "RANDOM":
        args = "".join(args).split(",")
        la = len(args)
        lae = len(args[0])
        if la == 2:
            arg1, t = __CONVERT_TO__(args[0].strip(), "int", cline, False, locals)
            if not t:
                arg1, t = __RUN__(arg1, cline, locals)

            arg2, t = __CONVERT_TO__(args[1].strip(), "int", cline, False, locals)
            if not t:
                arg2, t = __RUN__(arg2, cline, locals)
        elif la == 1 and lae != 0:
            arg1 = 0

            arg2, t = __CONVERT_TO__(args[0].strip(), "int", cline, False, locals)
            if not t:
                arg2, t = __RUN__(arg2, cline, locals)
        elif la == 0 or lae == 0:
            arg1 = 0
            arg2 = 32767
        else:
            return __DEBUG_INFO__("ERROR", True, True, f"RANDOM MUST RECIEVE BETWEEN 0 AND 2 ARGUMENTS, {la} PROVIDED")
        rval = random.randint(arg1, arg2)
        return [rval, True]

    # LEAVE THE CODE
    elif c == "EXIT":
        return ["leave", True]

    # RAISE ERROR IF __RUN__ NOT CALLED INTERNALLY BY INTERPRETER
    else:
        func:Func = None
        argss:list = []
        fargs:dict = {}
        func, argss = __PARSE_FUNC__(method if method else c if len(args) == 0 else c+"("+" ".join(args)+")", False, cline, locals, c.strip() if method else None)
        if not argss:
            func, args2 = __PARSE_FUNC__(__LINE__, False, cline, __GLOBALS__, c.strip() if method else None)

        if func:
            currentFile = func.file

        if argss:
            lineBeforeCallingFunc = cline
            for i, arg in enumerate(argss):
                arg, succ = __CONVERT_TO__(arg,  "*", cline, True, locals)
                fargs[f"arg{i}"] = arg

            shouldRunFuncCode = True

            return __RUN_FUNC__(fargs, func, locals, c if method else None)
        elif args2:
            lineBeforeCallingFunc = cline
            for i, arg in enumerate(args2):
                arg, succ = __CONVERT_TO__(arg,  "*", cline, True, locals)
                fargs[f"arg{i}"] = arg

            shouldRunFuncCode = True

            return __RUN_FUNC__(fargs, func, locals, c if method else None)
        
        try:
            return [locals[__LINE__], True]
        except KeyError:
            pass

        if '[' in c and c.split("[")[0][-1] != ' ':
            return __SEARCH_DICT_LIST__(c, locals, currentFile, cline)
        
        try:
            return [locals["__VARIABLES__"][currentFile][c].value, True]
        except KeyError:
            pass

        if warn:
            return __DEBUG_INFO__(level="ERROR", write_to_file=True, message=f"KEYWORD {__LINE__.split(' ')[0]} IS NOT DEFINED", line=cline)
        return [__LINE__, False]

def __PRE_RUN__(command:str, lines:list, locals:dict, optline:int=0, topre:list=[]):
    """
        EXECUTES CODE LIKE THE INTERPRET FUNCTION BUT AIMS FOR SPECIFIC PROVIDED COMMANDS
    """
    global currentFile, __GLOBALS__
    tmp = optline
    status = None
    try:
        if not __GLOBALS__["__FUNCTIONS__"][currentFile]:
            __GLOBALS__["__FUNCTIONS__"] = {currentFile: {}}
    except KeyError:
        __GLOBALS__["__FUNCTIONS__"] = {currentFile: {}}

    while tmp < len(lines):
        line = lines[tmp]
        line = line.split("-->")[0]

        if (line.startswith("END") and command == "FUNCTION") or(line.startswith(command)) or (command == "*" and not line.startswith("-->") and line != "\n" and not line.startswith("#DEFINE")) and not (line in topre):
            status, succ = __RUN__(line, tmp+1, locals)

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

def __RUN_FUNC__(args:dict, func:Func, locals:dict, scope:str="local"):
    """
        CREATES A NEW LOCAL AND EXECUTE CODE WITHIN A SET OF LINES PROVIDED BY func.line INSIDE func.file
    """
    global __TO_PRERUN__
    __LOCALS__ = __CREATE_LOCALS__(locals)

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

                try:
                    if line.startswith("RETURN") and (not shouldSkipCode[-1] or line.startswith("ELSE") or line.startswith("ENDIF") or line.startswith("IF")):
                        funcReturn, succ = __RUN__(line, cline+1, __LOCALS__, True)
                        break
                except IndexError:
                    if line.startswith("RETURN") and (line.startswith("ELSE") or line.startswith("ENDIF") or line.startswith("IF")):
                        funcReturn, succ = __RUN__(line, cline+1, __LOCALS__, True)
                        break

                if insideFunc > 0 and not shouldRunFuncCode:
                    should_run = line.startswith("FUNCTION") or line.startswith("END")

                try:
                    if should_run and (not shouldSkipCode[-1] or line.startswith("ELSE") or line.startswith("ENDIF") or line.startswith("IF")):
                        status, succ = __RUN__(line, cline+1, __LOCALS__, True, cline == func.line)
                except IndexError:
                    if should_run and (line.startswith("ELSE") or line.startswith("ENDIF") or line.startswith("IF")):
                        status, succ = __RUN__(line, cline+1, __LOCALS__, True, cline == func.line)

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
                    line = status[5:]
                    line, succ = __CONVERT_TO__(line, "int", cline, True, locals)
                    if not succ:
                        __DEBUG_INFO__("CRITICAL", True, True, "INNER ERROR WHILE TRYING TO SKIP LINE", cline)
                        break
                    else:
                        cline = line
                        continue

            cline+=1

    return [funcReturn, True]

def interpret():
    """
        EXECUTES EVERY FILE PROVIDED AS AN ARGUMENT TO THE INTERPRETER
    """
    global __GLOBALS__, insideFunc, shouldRunFuncCode, __TO_PRERUN__, currentFile
    for __FILE__ in __GLOBALS__["__FILES__"]:
        __GLOBALS__["__PROGRAM_FILEPATH__"] = __FILE__
        __GLOBALS__["__FILE_LINES__"] = {}
        __GLOBALS__["__FILE_LINES__"]["local"], succ = __OPEN_FILE__(__FILE__)
        lines = __GLOBALS__["__FILE_LINES__"]["local"]
        verfile = __FILE__+":version"
        filever = None
        status = True

        __PRE_RUN__("#DEFINE", lines, __GLOBALS__)

        try:
            with open(verfile, "r") as f:
                filever = f.read().strip()
        except FileNotFoundError:
            if not __GLOBALS__["__IGNORE_WARNS__"]:
                print(f"[WARN] THIS FILE DOES NOT CONTAIN A VERSION HEADER, THEREFORE IT MAY NOT RUN CORRECTLY ON THE CURRENT VERSION ({__CURRENT_VERSION__})")
                print(f"[INFO] SIGNING WITH RECENT VERSION TO SILENCE THE WARNING...")
                with open(verfile, "w") as f:
                    f.write(str(__CURRENT_VERSION__))
                time.sleep(2)

        if filever and not __GLOBALS__["__IGNORE_WARNS__"]:
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

                    try:
                        if should_run and (not shouldSkipCode[-1] or line.startswith("ELSE") or line.startswith("ENDIF") or line.startswith("IF")):
                            #print("Rodando: "+line.strip() + "\t\t\t"+str(cline+1))
                            status, succ = __RUN__(line, cline+1, __GLOBALS__)
                    except IndexError:
                        if should_run and (line.startswith("ELSE") or line.startswith("ENDIF") or line.startswith("IF")):
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

def console():
    """
        RUNS CODE INLINE BUT DOES NOT SUPPORT CODES THAT VOLUNTARILY SKIPS LINES
    """
    global __CONSOLE_MODE__
    unsupported = ["LABEL", "JUMP", "IF", "ELSE", "FUNCTION", "END"]
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
            __GLOBALS__["__FILE_LINES__"]["local"].append(keyword)
            if keyword.split()[0] in unsupported:
                print(f"> [INFO] UNFORTUNATELY, THE CONSOLE MODE CANNOT EXECUTE {keyword.split()[0]} OPERATIONS")
            elif keyword.endswith(".mow"):
                tmplines, succ = __OPEN_FILE__(keyword)
                if succ:
                    print(f"> [INFO] RUNNING FILE {keyword}")
                    __CONSOLE_MODE__ = False
                    __GLOBALS__["__FILES__"].append(keyword)
                    __GLOBALS__["__FILE_LINES__"] = {}
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