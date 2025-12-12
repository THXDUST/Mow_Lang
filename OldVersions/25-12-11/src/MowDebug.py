
from colorama import Fore, init
from MowTypes import Token

init(autoreset=True)

class MowLangWarning(Warning):
    def __init__(self, message):
        self.message = message
    
    def __str__(self):
        return f"MowLangWarning: {self.message}"

def MowLangError(Message:str, Token:Token=Token("", "", 1, 1), FilePath:str="", Lines:list[str]=[], QuitWithMessage:bool=True):
    if QuitWithMessage:
        try:
            print(Fore.LIGHTRED_EX + f"""\n
        <---------- ERROR REPORT ---------->
         MowLang File: "{FilePath}", line {Token.line}, char {Token.char_pos}:
         |{Lines[Token.line-1].strip()}|
{" "*(Token.char_pos+9-(len(Lines[Token.line-1])-len(Lines[Token.line-1].strip()))) + "^" * max(1, len(Token.aux) if Token.type != "EOF" else 1)}

         Error during execution:
            {Message}
        <---------------------------------->\n""")
        except Exception as e:
            print(Fore.LIGHTRED_EX + f"""\n
        <---------- ERROR REPORT ---------->
         MowLang File: "{FilePath}", line {Token.line}, char {Token.char_pos}:
         |{Lines[Token.line-1].strip()}|

         Error during execution:
            {Message}

            Error occured while generating
            the report

            Look in your code for syntax
            errors

            OR

            Error happened somewhere near
            a number value
        <---------------------------------->\n""")
            
        quit()
    return False

def MowLangQuit(Message:str, ExitCode:int=0, Globals:dict={}, Line: int=1, CharPos: int = 1):
    try:
        ExitCode = str(ExitCode)
    except ValueError:
        from MowErrors import ThrowRuntimeError
        ThrowRuntimeError("Exit code way too long", Token("integer_literal", "1", Line, CharPos, 0), Globals)
    print(f"""\n
    <----- PROGRAM QUIT ----->
     {Message}
     Program Left With Exit Code: {ExitCode}
    <------------------------>\n""")
    quit()