import warnings

class NotNull:
    value: str = "NotNull"

    def __str__(self):
        return "<NotNullClass>"
    
    def __repr__(self):
        return "<NotNullClass>"

class FunctionParam:
    def __init__(self, name: str, type: str, default: any = NotNull()):
        self.name = name
        self.type = type
        self.default = default
    
    def __repr__(self):
        return f"({self.name}: {self.type}{f' = {self.default}' if self.default != NotNull() and self.default != None else ''})"

class Function:
    def __init__(self, name:str, parameters:list[FunctionParam], body:list, data_type: str, static:bool = False):
        self.name = name
        self.parameters = parameters
        self.body = body
        self.static = static
        self.type = data_type
    
    def __repr__(self):
        params = ', '.join(f'{p.name}: {p.type}{f" = {p.default}" if p.default != NotNull() else ""}' for p in self.parameters)
        return f"{'CONST' if self.static else ''} {self.type} {self.name}({params})"

class Variable:
    def __init__(self, name:str, value: any, is_const:bool, type:str):
        self.name = name
        self.value = value
        self.const = is_const
        self.type = type
    
    def __repr__(self):
        t = f'"{self.value}"'
        return f"{'CONST' if self.const else ''} {self.type.upper()} {self.name} = {t if self.type in ('STR', 'CHAR') else self.value}"

class Token:
    def __init__(self, Type, Value, Line:int=1, CharPos:int=1, Depth:int=0, AuxVal:any = ""):
        self.type:str = Type
        self.value:any = Value
        self.line:int = Line
        self.char_pos:int = CharPos
        self.depth:int = Depth

        self.aux:any = AuxVal
    
    def __repr__(self):
        return f"Token(Val: {self.value}\t\tType: {self.type}\tAuxVal: {self.aux}\t(l: {self.line}, c: {self.char_pos}, z: {self.depth}))"
    
    def __str__(self):
        return f"Token(Val: {self.value}\t\tType: {self.type}\tAuxVal: {self.aux}\t(l: {self.line}, c: {self.char_pos}, z: {self.depth}))"

class Stack:
    def __init__(self, Hash_Size: int, Line: int, Globals: dict, Token: Token):
        from MowErrors import ThrowNameError, ThrowValueError, ThrowTypeError, ThrowNotImplementedError, ThrowRuntimeError, ThrowSyntaxError
        from MowDebug import MowLangWarning

        if not Globals["VARIABLES"]["StackDeprecationNoWarnings"].value:
            MowLangWarning("Stack is pending deprecation, consider using regular variables instead. If you don't want to see this warning anymore use 'DEFINE StackDeprecationNoWarning TRUE' to silence this warning", Token, Globals["FILE"], Globals["LINES"])

        if Hash_Size == 0:
            ThrowRuntimeError("Cannot start Stack with hash size of 0", Token, Globals)
        elif Hash_Size < 0:
            ThrowRuntimeError("Cannot start Stack with negative hash size", Token, Globals)

        self.buf = [0 for _ in range(Hash_Size)]
        self.pointer = -1
        self.hash_size = Hash_Size

    def push(self, Value: any, Globals: dict, Token: Token):
        from MowErrors import ThrowRuntimeError

        try:
            self.pointer += 1
            self.buf[self.pointer] = Value
            return self.buf[self.pointer]
        except IndexError:
            ThrowRuntimeError("Cannot set value to a stack index out of range", Token, Globals)
        except Exception as e:
            ThrowRuntimeError(str(e), Token, Globals)

    def pop(self, Globals: dict, Token: Token):
        from MowErrors import ThrowRuntimeError

        try:
            val = self.buf[self.pointer]
            self.buf[self.pointer] = 0
            self.pointer -= 1
            return val
        except IndexError:
            ThrowRuntimeError("Cannot pop an empty Stack", Token, Globals)
        except Exception as e:
            ThrowRuntimeError(str(e), Token, Globals)

    def top(self):
        return self.buf[self.pointer]
    
    def print(self):
        print(self.buf[:self.pointer+1])
    
    def printall(self):
        print(self.buf)

    def clear(self):
        while self.pointer > -1:
            self.buf[self.pointer] = 0
            self.pointer-=1

    def destroy(self):
        del self.buf

    def swap(self, Globals: dict, Token: Token):
        from MowErrors import ThrowRuntimeError

        try:
            self.buf[self.pointer], self.buf[self.pointer-1] = self.buf[self.pointer-1], self.buf[self.pointer]
        except IndexError:
            ThrowRuntimeError("Cannot swap value in Stack filled with none of 1 value", Token, Globals)
        except Exception as e:
            ThrowRuntimeError(str(e), Token, Globals)

    def dup(self, Globals: dict, Token: Token):
        from MowErrors import ThrowRuntimeError

        try:
            self.buf[self.pointer+1] = self.buf[self.pointer]
            self.pointer+=1
        except IndexError:
            ThrowRuntimeError("Tried to dupe outside range of Stack", Token, Globals)
        except Exception as e:
            ThrowRuntimeError(str(e), Token, Globals)

    def rot(self, Globals: dict, Token: Token):
        from MowErrors import ThrowRuntimeError

        try:
            self.buf = self.buf[::-1]
        except IndexError:
            ThrowRuntimeError("Tried to rotate empty Stack", Token, Globals)
        except Exception as e:
            ThrowRuntimeError(str(e), Token, Globals)

class Label:
    def __init__(self, name: str, token_pos: int):
        self.name = name
        self.pos = token_pos

    def __str__(self):
        return f"LABEL(\"{self.name}\"); // {self.pos}"
    
class Class:
    def __init__(self, name: str, methods: dict, properties: dict, parent_class=None):
        self.name = name
        self.methods = methods
        self.properties = properties
        self.parent_class = parent_class
        self.instances = {}
    
    def __repr__(self):
        methods_str = ', '.join(self.methods.keys())
        props_str = ', '.join(self.properties.keys())
        return f"CLASS {self.name} (methods: [{methods_str}], properties: [{props_str}])"

class Instance:
    def __init__(self, class_def: Class, instance_id: str):
        self.class_def = class_def
        self.instance_id = instance_id
        self.attributes = {}
        
        for prop_name, (prop_type, _) in class_def.properties.items():
            self.attributes[prop_name] = self._get_default_value(prop_type)
    
    def _get_default_value(self, type_name: str):
        defaults = {
            "INT": 0,
            "FLOAT": 0.0,
            "STR": "",
            "BOOL": False,
            "LIST": [],
            "DICT": {},
            "CHAR": '\0',
            "VOID": None
        }
        return defaults.get(type_name.upper(), None)
    
    def __repr__(self):
        attrs_str = ', '.join([f"{k}={v}" for k, v in self.attributes.items()])
        return f"Instance of {self.class_def.name} ({attrs_str})"