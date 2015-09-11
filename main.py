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
