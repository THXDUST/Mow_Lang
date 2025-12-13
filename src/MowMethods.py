class BuiltInMethods:
    """Gerencia métodos nativos para tipos de dados em MowLang"""
    
    @staticmethod
    def get_string_methods():
        """Retorna métodos disponíveis para strings"""
        return {
            "UPPER": lambda s: s.upper(),
            "LOWER": lambda s: s.lower(),
            "LENGTH": lambda s: len(s),
            "TRIM": lambda s: s.strip(),
            "STRIP": lambda s: s.strip(),
            "REVERSE": lambda s: s[::-1],
            "REPLACE": lambda s, old, new: s.replace(old, new),
            "SPLIT": lambda s, sep=" ": s.split(sep),
            "STARTSWITH": lambda s, prefix: "TRUE" if s.startswith(prefix) else "FALSE",
            "ENDSWITH": lambda s, suffix: "TRUE" if s.endswith(suffix) else "FALSE",
            "CONTAINS": lambda s, substr: "TRUE" if substr in s else "FALSE",
            "FIND": lambda s, substr: s.find(substr),
            "COUNT": lambda s, substr: s.count(substr),
            "ISDIGIT": lambda s: "TRUE" if s.isdigit() else "FALSE",
            "ISALPHA": lambda s: "TRUE" if s.isalpha() else "FALSE",
            "ISALNUM": lambda s: "TRUE" if s.isalnum() else "FALSE",
            "ISSPACE": lambda s: "TRUE" if s.isspace() else "FALSE",
        }
    
    @staticmethod
    def get_list_methods():
        """Retorna métodos disponíveis para listas"""
        return {
            "APPEND": lambda lst, item: (lst.append(item), lst)[1],
            "POP": lambda lst, idx=-1: lst.pop(idx),
            "REMOVE": lambda lst, item: (lst.remove(item), lst)[1],
            "INSERT": lambda lst, idx, item: (lst.insert(idx, item), lst)[1],
            "CLEAR": lambda lst: (lst.clear(), lst)[1],
            "LENGTH": lambda lst: len(lst),
            "REVERSE": lambda lst: (lst.reverse(), lst)[1],
            "SORT": lambda lst: (lst.sort(), lst)[1],
            "INDEX": lambda lst, item: lst.index(item),
            "COUNT": lambda lst, item: lst.count(item),
            "COPY": lambda lst: lst.copy(),
            "CONTAINS": lambda lst, item: "TRUE" if item in lst else "FALSE",
        }
    
    @staticmethod
    def get_dict_methods():
        """Retorna métodos disponíveis para dicionários"""
        return {
            "KEYS": lambda d: list(d.keys()),
            "VALUES": lambda d: list(d.values()),
            "ITEMS": lambda d: [(k, v) for k, v in d.items()],
            "GET": lambda d, key, default=None: d.get(key, default),
            "POP": lambda d, key, default=None: d.pop(key, default),
            "CLEAR": lambda d: (d.clear(), d)[1],
            "UPDATE": lambda d, other: (d.update(other), d)[1],
            "LENGTH": lambda d: len(d),
            "COPY": lambda d: d.copy(),
            "CONTAINS": lambda d, key: 1 if key in d else 0,
            "REMOVE": lambda d, key: (d.pop(key, None), d)[1],
        }
    
    @staticmethod
    def get_int_methods():
        """Retorna métodos disponíveis para inteiros"""
        return {
            "ABS": lambda n: abs(n),
            "STR": lambda n: str(n),
            "FLOAT": lambda n: float(n),
            "BIT_LENGTH": lambda n: n.bit_length() if n >= 0 else (n + 1).bit_length(),
            "TO_HEX": lambda n: hex(n),
            "TO_BIN": lambda n: bin(n),
            "TO_OCT": lambda n: oct(n),
        }
    
    @staticmethod
    def get_float_methods():
        """Retorna métodos disponíveis para floats"""
        return {
            "ABS": lambda n: abs(n),
            "STR": lambda n: str(n),
            "INT": lambda n: int(n),
            "ROUND": lambda n, digits=0: round(n, digits),
            "CEIL": lambda n: __import__('math').ceil(n),
            "FLOOR": lambda n: __import__('math').floor(n),
        }
    
    @staticmethod
    def get_bool_methods():
        """Retorna métodos disponíveis para booleanos"""
        return {
            "STR": lambda b: "TRUE" if b else "FALSE",
            "INT": lambda b: 1 if b else 0,
        }
    
    @staticmethod
    def call_method(base_value, method_name, args, token, globals_dict):
        """
        Executa um método em um valor base
        
        Args:
            base_value: O valor sobre o qual o método será chamado
            method_name: Nome do método
            args: Lista de argumentos
            token: Token para mensagens de erro
            globals_dict: Dicionário global para contexto de erro
        
        Returns:
            O resultado da execução do método
        """
        from MowErrors import ThrowNameError, ThrowTypeError, ThrowRuntimeError
        
        method_dict = None
        value_type = "UNKNOWN"
        
        # Determinar tipo e obter métodos disponíveis
        if isinstance(base_value, str):
            method_dict = BuiltInMethods.get_string_methods()
            value_type = "STR"
        elif isinstance(base_value, list):
            method_dict = BuiltInMethods.get_list_methods()
            value_type = "LIST"
        elif isinstance(base_value, dict):
            method_dict = BuiltInMethods.get_dict_methods()
            value_type = "DICT"
        elif isinstance(base_value, int) and not isinstance(base_value, bool):
            method_dict = BuiltInMethods.get_int_methods()
            value_type = "INT"
        elif isinstance(base_value, float):
            method_dict = BuiltInMethods.get_float_methods()
            value_type = "FLOAT"
        elif isinstance(base_value, bool):
            method_dict = BuiltInMethods.get_bool_methods()
            value_type = "BOOL"
        
        if method_dict is None:
            ThrowTypeError(f"Type '{value_type}' does not support method calls", token, globals_dict)
        
        if method_name not in method_dict:
            ThrowNameError(f"Method '{method_name}' not found for type '{value_type}'", token, globals_dict)
        
        try:
            method = method_dict[method_name]
            if args:
                return method(base_value, *args)
            else:
                return method(base_value)
        except TypeError as e:
            if "positional argument" in str(e):
                ThrowRuntimeError(f"Method '{method_name}' called with incorrect number of arguments: {str(e)}", token, globals_dict)
            else:
                ThrowRuntimeError(f"Error calling method '{method_name}': {str(e)}", token, globals_dict)
        except Exception as e:
            ThrowRuntimeError(f"Error executing method '{method_name}': {str(e)}", token, globals_dict)
