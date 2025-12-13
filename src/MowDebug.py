
from colorama import Fore, init
from MowTypes import Token
import sys

init(autoreset=True)
    
def MowLangWarning(Message:str, Token:Token, Filepath:str, Lines:list[str]=[' ']):
    print(Fore.YELLOW + f"""
        <---------- Warning ---------->
         MowLang File: "{Filepath}", Line {Token.line}, Char {Token.char_pos}:
         |{Lines[Token.line-1].strip()}|
{" "*(Token.char_pos+9-(len(Lines[Token.line-1])-len(Lines[Token.line-1].strip()))) + "^" * max(1, len(Token.aux) if Token.type != "EOF" else 1)}

         Warning: {Message}
        <----------------------------->""")

def MowLangError(Message:str, Token:Token=Token("", "", 1, 1), FilePath:str="", Lines:list[str]=[' '], QuitWithMessage:bool=True):
    if QuitWithMessage:
        try:
            print(Fore.LIGHTRED_EX + f"""\n
        <---------- ERROR REPORT ---------->
         MowLang File: "{FilePath}", Line {Token.line}, Char {Token.char_pos}:
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
            the report.
        <---------------------------------->\n""")
            
        sys.exit(1)
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
    sys.exit(0)