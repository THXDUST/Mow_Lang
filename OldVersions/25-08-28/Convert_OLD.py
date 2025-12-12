import os

translated = []
file = ""
file_lines = []

def __is_string(literal:str, offset=0):
    parts = literal.split(None, 1+offset)
    if len(parts) <= 1+offset:
        return ["", False]
    lit = parts[1+offset].rstrip("\n")
    is_string = lit.startswith('"') and lit.endswith('"')
    return [lit, is_string]

def translatorOLD(file):
    global file_lines, translated
    with open(file, encoding="utf-8", mode="r") as f:
        file_lines = f.readlines()
    offset = 0
    for index, line in enumerate(file_lines):
        cline = index+1
        if line.startswith("ALLOC"):
            line = line[6:]
            arg = line.split("\n")[0]
            translated.append("STACK.ALLOC " + arg + "\n")
        elif line.startswith("READ"):
            print(f"[INFO] NEED TO DEFINE READ DATATYPE, USING DEFAULT: STRING. AT LINE {cline+offset}")
            translated.append("STACK.PUSH str READ str \"DEFINE DATATYPE FOR READ\"\n")
        elif line == "\n":
            translated.append("\n")
        elif line.startswith("JUMP"):
            keyword = line.split(" ")[0]
            label = line[len(keyword)+1:].split("\n")[0]
            args = " ".join(keyword.split("_"))
            args = " ".join(args.split("."))
            translated.append(args + " " + label + "\n")
        elif line.startswith("EXIT"):
            translated.append("EXIT\n")
        elif line.strip("\n").endswith(":"):
            label = line.split(":")[0]
            translated.append("LABEL " + label + "\n")
        elif line.strip("\n") == "PRINT.STACK":
            translated.append("STACK.PRINT\n")
        elif line.startswith("ADD"):
            translated.append("STACK.PUSH float ADD STACK STACK\n")
        elif line.startswith("SUB"):
            translated.append("STACK.PUSH float SUB STACK STACK\n")
        elif line.startswith("MUL"):
            translated.append("STACK.PUSH float MUL STACK STACK\n")
        elif line.startswith("DIV"):
            translated.append("STACK.PUSH float DIV STACK STACK\n")
        elif line.startswith("MOD"):
            translated.append("STACK.PUSH float MOD STACK STACK\n")
        elif line.startswith("EXP"):
            translated.append("STACK.PUSH float EXP STACK STACK\n")


        elif line.startswith("PUSH"):
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
                warn = "\n--> DEFINE VALUE TYPE, DEFAULT: STRING\n"
            translated.append(f"{warn}STACK.PUSH {vartype} {arg}\n")
        elif line.startswith("PRINT"):
            if line.endswith("\n"):
                line = line[:-1]
            arg = line[6:]
            arg, is_string = __is_string(arg.strip("\n"), -1)
            if not is_string:
                if arg == "1":
                    translated.append("PRINT STACK.TOP\n")
                elif arg == "STACK":
                    translated.append("STACK.PRINT\n")
                elif arg == "VARS":
                    translated.append("PRINT.VARS\n")
            else:
                offset+=2
                print(f"[INFO] MAKE SURE THE THING THAT THE PROGRAM TRANSLATED FINE AT THE LINE {cline+offset}")
                translated.append(f"\n--> MAKE SURE THE PRINT IS RIGHT HERE\nPRINT {arg}\n")
        elif line.startswith("-->"):
            translated.append(line)
        elif line.startswith("CLEAR"):
            translated.append("STACK.CLEAR\n")
        elif line.startswith("STORE"):
            name = line[6:]
            offset+=2
            print(f"[INFO] MAKE SURE TO SET THE RIGHT DATATYPE TO STORE ON LINE {cline+offset}")
            translated.append(f"\n--> MAKE SURE TO SET THE RIGHT DATATYPE HERE\nSTORE str {name}\n")
        elif line.startswith("LOAD"):
            name = line[5:]
            translated.append(f"LOAD {name}")


        else:
            offset+=2
            print(f"[ERROR] ERROR WHILE TRANSLATING {file.split('/')[-1]}, KEYWORD {line.split()[0]} IS NOT DEFINED. APPENDING RAW FORMAT. AT LINE {cline+offset}")
            translated.append("\n--> NOT TRANSLATED:\n"+line)
    
    newfile = file[:-4]+".new.mow"
    filever = newfile+":version"
    with open(newfile, "w", encoding="utf-8") as f:
        f.write("".join(translated))
        f.close()
    with open(filever, "w", encoding="utf-8") as f:
        f.write("1.0.0")
        f.close()

    

translatorOLD("example5.mow")
#print("\n\n", translated)