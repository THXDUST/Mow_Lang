import sys
sys.path.insert(0, 'src')
from mowlang import Tokenize

with open('test_reassign2.mow') as f:
    code = f.read()

tokens = Tokenize(code)
for i, t in enumerate(tokens):
    if i >= 18 and i <= 30:
        print(f"{i}: {t.type:20} aux={getattr(t, 'aux', None)!s:10} value={getattr(t, 'value', None)}")
