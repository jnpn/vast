'''
Mostly a Python to (Emacs)Lisp pretty printer.

Many AST visitors have been implemented. The useful one
being Elispy, which has a fallback to generic_visit for
cases not handled yet.
'''

import ast

from vast.vast import Source
from vast.visitors.visitors import Elispy
from vast.transformers.transformer import Desugar
from snippets import snippets

def premain(visitor):
    '''Parse test snippets and pass them to visitor.'''
    for name, source in snippets.items():
        fn = name.capitalize()
        src = Source(fn).of(source).into(visitor)
        qs, qt = src.transpile()
        qv = src.emacs_eval()
        print(qs)
        print(qt)
        print(qv)
        print()

def repl():
    while True:
        print(end='>>> ')
        exp = input()
        if exp == 'q' or exp == 'Q' or exp == 'quit' or exp == 'Quit' or exp == 'QUIT':
            break
        else:
            (py,el) = Source().of(exp).into(Elispy).transpile()
            print('; =>', el)
    print('bye.')

def oldmain():
    print('-- Elispy')
    print('--  Python to Elisp pretty printer')

    s = 'def f(x): return x+1'
    a = ast.parse(s)
    b = Desugar().visit(a)
    t = Elispy().visit(b)
    print(t)

    c = Source(fn='increment (cps)').of(s).nanopass(Desugar).into(Elispy)
    qs, qt = c.transpile()
    print(qs)
    print(qt)
    # print(ast.dump(b))
    # main()


def main():
    '''Helper, calls premain(Elispy).'''
    return premain(Elispy)


def transform(filename):
    s = Source().load(filename).into(Elispy)
    qs, qt = s.transpile()
    print(qs)
    print(qt)

# Main

if __name__ == '__main__':
    repl()
