translated = []
file = ""
file_lines = []
cver = "1.3.0"

try:
    with open("VERSION") as f:
        data = f.readlines()
        cver = data[0].strip()
except FileNotFoundError:
    pass



def __is_string(literal:str, offset=0):
    parts = literal.split(None, 0+offset)
    if len(parts) <= 0+offset:
        return ["", False]
    lit = parts[0+offset].rstrip("\n")
    is_string = (lit.startswith('"') and lit.endswith('"')) or (lit.startswith("'") and lit.endswith("'"))
    return [lit, is_string]

def translatorOLD(filepath: str):
    global file_lines, translated
    translated = []
    file_lines = []
    with open(filepath, encoding="utf-8", mode="r") as f:
        file_lines = f.readlines()
    offset = 0
    indent = 0
    fver = False
    try:
        with open(filepath+":version", "r") as f:
            fver = f.read().strip()
    except FileNotFoundError:
        fver = "0.0.1"

    if not fver:
        return ["CRITICAL", "FILE TRANSLATOR CANNOT TRANSLATE FILE WITH NO VERSION PROPERTY", False, file_lines]

    for index, rawline in enumerate(file_lines):
        cline = index+1
        line = rawline.split("-->")[0].strip()
        if line == '':
            continue

        try:
            comment = "\t--> "+rawline.split("-->")[1].strip()
        except IndexError:
            comment = ""


        c = line.split()[0].split(".")[0].split("(")[0].strip()
        try:
            method = line.split()[0].split('.')[1].strip()
        except IndexError:
            method = False
        
        try:
            args = line.split(c, 1)[1].strip().split()
        except IndexError:
            args = False

        if c == "ALLOC":
            if fver == "0.0.1":
                if method == "DESTROY":
                    translated.append('\t'*indent+"STACK.DESTROY")
                else:
                    translated.append('\t'*indent+"STACK.ALLOC " + args[0] if args else '1')
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "READ":
            if fver == "0.0.1":
                print(f"[INFO] NEED TO DEFINE READ DATATYPE, USING DEFAULT: STRING. AT LINE {cline+offset}")
                translated.append('\t'*indent+"STACK.PUSH str READ str \"DEFINE DATATYPE FOR READ\"")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "JUMP":
            if fver == "0.0.1":
                label = args[0]
                args = c.split("_")
                args.insert(2, "STACK.TOP,")
                args = " ".join(args)
                translated.append('\t'*indent+args + " " + label)
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "EXIT":
            translated.append('\t'*indent+"EXIT")
        elif c.endswith(":"):
            if fver == "0.0.1":
                label = c.strip(":")
                translated.append('\t'*indent+"LABEL " + label)
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "ADD":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH float ADD STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "SUB":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH float SUB STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "MUL":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH float MUL STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "DIV":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH float DIV STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "MOD":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH float MOD STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "EXP":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH float EXP STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "PUSH":
            if fver == "0.0.1":
                arg, is_string = __is_string(line[5:].split(" ", 1)[0].strip("\n"), -1)
                vartype = "str"
                warn = ""
                try:
                    if int(arg) == float(arg):
                        arg = int(arg)
                        vartype = "int"
                    else:
                        arg = float(arg)
                        vartype = "float"
                except ValueError:
                    offset+=2
                    arg = str(arg)
                    vartype = "str"
                    print(f"[INFO] NEED TO DEFINE PUSH DATATYPE, USING DEFAULT: STRING. AT LINE {cline+offset}")
                    warn = "\n"+'\t'*indent+"--> DEFINE VALUE TYPE, DEFAULT: STRING\n"
                translated.append(warn+'\t'*indent+f"STACK.PUSH {vartype} {arg}")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "PRINT" or c == "PRINTL":
            if fver == "0.0.1":
                if not method:
                    arg, is_string = __is_string(" ".join(args))
                    if not is_string:
                        if arg[0] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']:
                            translated.append('\t'*indent+"PRINTL STACK.TOP\n")
                        else:
                            translated.append('\t'*indent+f"PRINTL {arg}")
                    else:
                        offset+=2
                        print(f"[INFO] MAKE SURE THE THING THAT THE PROGRAM TRANSLATED FINE AT THE LINE {cline+offset}")
                        translated.append('\t'*indent+"\n--> MAKE SURE THE PRINT IS RIGHT HERE"+'\t'*indent+f"\nPRINTL {arg}")
                elif method == "VARS":
                    translated.append('\t'*indent+"PRINTL.VARS")
                elif method == "STACK":
                    translated.append('\t'*indent+"STACK.PRINT")
            elif fver == cver:
                translated.append('\t'*indent+line)

        elif rawline.strip().startswith("-->"):
            translated.append('\t'*indent+rawline)
        elif c == "CLEAR":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.CLEAR")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "POP":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "NEG":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH int NEG STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "INC":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH int INC STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "DEC":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH int DEC STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "ABS":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH int ABS STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "SQRT":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH float SQRT STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "MAX":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH int MAX STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "MIN":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH int MIN STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "CMP":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.PUSH bool EQ STACK.POP, STACK.POP")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "STORE":
            if fver == "0.0.1":
                name = args[0] if args else None
                if not name:
                    print(f"[CRITICAL] ERROR WHILE TRANSLATING {filepath.split('/')[-1]}, KEYWORD {c} REQUIRES THE NAME OF THE VARIABLE TO STORE. APPENDING RAW LINE {line}. AT LINE {cline+offset}")
                    translated.append('\t'*indent+line)
                offset+=2
                print(f"[INFO] MAKE SURE TO SET THE RIGHT DATATYPE TO STORE ON LINE {cline+offset}")
                translated.append('\t'*indent+"\n--> MAKE SURE TO SET THE RIGHT DATATYPE HERE"+'\t'*indent+f"\nSTORE str {name}\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "LOAD":
            if fver == "0.0.1":
                name = args[0] if args else None
                if not name:
                    print(f"[CRITICAL] ERROR WHILE TRANSLATING {filepath.split('/')[-1]}, KEYWORD {c} REQUIRES THE NAME OF THE VARIABLE TO LOAD. APPENDING RAW LINE {line}. AT LINE {cline+offset}")
                    translated.append('\t'*indent+line)
                offset+=1
                print(f"[INFO] MAKE SURE THE PROGRAM TRANSLATED FINE AT THE LINE {cline+offset}")
                translated.append('\t'*indent+"--> MAKE SURE THE DATA TYPE IS RIGHT HERE"+'\t'*indent+f"\nSTACK.PUSH str LOAD {name}\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "RANDOM":
            if fver == "0.0.1":
                translated.append('\t'*indent+f"STACK.PUSH int RANDOM {', '.join(args)}\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "ROT":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.ROT\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "SWAP":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.SWAP\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "DUP":
            if fver == "0.0.1":
                translated.append('\t'*indent+"STACK.DUP\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "TIME":
            if fver == "0.0.1":
                app = ""
                for arg in args:
                    arg = arg.strip()
                    if arg == "UTC":
                        app = "GLOBAL " + app
                    elif arg == "LOCAL":
                        app = "LOCAL " + app
                    elif arg == "FULL":
                        app += " %H:%M:%S.%f %m-%d-%Y"
                    elif arg == "YEAR":
                        app += " %Y"
                    elif arg == "MONTH":
                        app += " %m"
                    elif arg == "DAY":
                        app += " %d"
                    elif arg == "HOUR":
                        app += " %H"
                    elif arg == "MINUTE":
                        app += " %M"
                    elif arg == "SECOND":
                        app += " %S"
                    elif arg == "MICROSECOND":
                        app += " %f"
                    
                    app = "TIME.NOW " + app
                translated.append('\t'*indent+"STACK.PUSH str "+ app + "\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "WAIT":
            if fver == "0.0.1":
                translated.append('\t'*indent+"TIME.SLEEP "+args[0]+"\n")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "IF":
            if fver == "0.0.1":
                if args.strip("==")[0] != args:
                    translated.append('\t'*indent+f"IF (EQ {args.split('==')[0].strip()}, {args.split('==')[1].strip()})")
                elif args.strip("<=")[0] != args:
                    translated.append('\t'*indent+f"IF (LTE {args.split('<=')[0].strip()}, {args.split('<=')[1].strip()})")
                elif args.strip(">=")[0] != args:
                    translated.append('\t'*indent+f"IF (GTE {args.split('>=')[0].strip()}, {args.split('>=')[1].strip()})")
                elif args.strip("<")[0] != args:
                    translated.append('\t'*indent+f"IF (LT {args.split('<')[0].strip()}, {args.split('<')[1].strip()})")
                elif args.strip(">")[0] != args:
                    translated.append('\t'*indent+f"IF (GT {args.split('>')[0].strip()}, {args.split('>')[1].strip()})")
                else:
                    translated.append('\t'*indent+line)
            elif fver == cver:
                translated.append('\t'*indent+line)
            indent+=1
        elif c == "END":
            indent-=1
            if fver == "0.0.1":
                translated.append('\t'*max(0, indent)+f"{line}{' '+args[0] if args else 'IF'}")
            elif fver == cver:
                translated.append('\t'*indent+line)
        elif c == "ELSE":
            translated.append('\t'*max(0, indent-1)+line)


        elif c == "":
            translated.append(line)
        else:
            offset+=2
            print(f"[ERROR] ERROR WHILE TRANSLATING {filepath.split('/')[-1]}, KEYWORD {c} IS NOT DEFINED. APPENDING RAW LINE {line}. AT LINE {cline+offset}")
            translated.append("\n--> NOT TRANSLATED:\n"+line)

        translated.append(translated.pop() + comment)
    
    if '/' in filepath:
        path = "/".join(filepath.split("/")[:-1])
    elif "\\" in filepath:
        path = "\\".join(filepath.split("\\")[:-1])
    else:
        path = "./"

    newfile = (filepath.split(path)[1] if path in filepath else filepath)[:-4]+".new.mow"
    filever = newfile+":version"
    with open(path+newfile, "w", encoding="utf-8") as f:
        f.write("\n".join(translated))
        f.close()
    with open(path+filever, "w", encoding="utf-8") as f:
        f.write(str(cver))
        f.close()
    
    return ["INFO", f"FILE TRANSLATED SUCCESSFULLY, RESULTS VISIBLE INSIDE {path+newfile}", True, translated]