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

import click

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

@click.group()
def cli():
    pass

def multiline(end=lambda inp: len(inp) < 1):
    "-> [str]"
    r = []
    l = input()
    while not end(l):
        r.append(l)
        l = input()
    return r

def is_exit(exp):
    return exp == 'q' or exp == 'Q' or exp == 'quit' or exp == 'Quit' or exp == 'QUIT'

@cli.command()
def repl():
    while True:
        print(end='>>> ')
        exp = multiline()
        if exp[0] and is_exit(exp[0]):
            break
        else:
            (py,el) = Source().of('\n'.join(exp)).into(Elispy).transpile()
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

@cli.command()
def compile(filename):
    s = Source().load(filename).into(Elispy)
    qs, qt = s.transpile()
    print(qs)
    print(qt)

# Main

if __name__ == '__main__':
    cli()
