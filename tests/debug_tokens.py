import sys
sys.path.insert(0, 'src')
from mowlang import Tokenize

code = """[10, 20, 30].append(40);"""

tokens = Tokenize(code)
for i, t in enumerate(tokens):
    print(f"{i}: {t.type:20} aux={getattr(t, 'aux', None)!s:10} value={getattr(t, 'value', None)}")
