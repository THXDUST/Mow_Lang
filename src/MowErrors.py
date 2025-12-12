from MowDebug import MowLangError
from MowTypes import Token

def ThrowRuntimeError(Message: str, Token: Token, Globals: dict):
    MowLangError(f"Runtime Error: {Message.strip()}", Token, Globals["FILE"], Globals["LINES"])

def ThrowValueError(Message: str, Token: Token, Globals: dict):
    MowLangError(f"Value Error: {Message.strip()}", Token, Globals["FILE"], Globals["LINES"])

def ThrowTypeError(Message: str, Token: Token, Globals: dict):
    MowLangError(f"Type Error: {Message.strip()}", Token, Globals["FILE"], Globals["LINES"])

def ThrowNameError(Message: str, Token: Token, Globals: dict):
    MowLangError(f"Name Error: {Message.strip()}", Token, Globals["FILE"], Globals["LINES"])

def ThrowSyntaxError(Message: str, Token: Token, Globals: dict):
    MowLangError(f"Syntax Error: {Message.strip()}", Token, Globals["FILE"], Globals["LINES"])

def ThrowNotImplementedError(Message: str, Token: Token, Globals):
    MowLangError(f"Not Implemented Error: {Message.strip()}", Token, Globals["FILE"], Globals["LINES"])

def ThrowKeyboardInterruptError(Message: str, Token: Token, Globals):
    MowLangError(f"Keyboard Interrupted Error: {Message.strip()}", Token, Globals["FILE"], Globals["LINES"])