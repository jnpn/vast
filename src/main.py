"""
Mostly a Python to (Emacs)Lisp pretty printer.

Many AST visitors have been implemented. The useful one
being Elispy, which has a fallback to generic_visit for
cases not handled yet.
"""

import ast

import click

from snippets import snippets
from vast.transformers.transformer import Desugar
from vast.vast import Source
from vast.visitors.visitors import Elispy


def premain(visitor):
    """Parse test snippets and pass them to visitor."""
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
    inp = input()
    while not end(inp):
        r.append(inp)
        inp = input()
    return r


def is_exit(exp):
    return exp in ["q", "Q", "quit", "Quit", "QUIT"]


@cli.command()
def repl():
    while True:
        print(end=">>> ")
        exp = multiline()
        if exp[0] and is_exit(exp[0]):
            break
        (_, el) = Source().of("\n".join(exp)).into(Elispy).transpile()
        print("; =>", el)
    print("bye.")


@cli.command()
@click.argument("filename")
def load_file(filename):
    src = Source().load(filename).into(Elispy)
    qs, qt = src.transpile()
    print(qs)
    print(qt)
    elisp_file = filename.replace(".py", ".el")
    with open(elisp_file, "w") as f:
        print(qt, file=f)
    print(f"Transpiled code saved to {elisp_file}")


def oldmain():
    print("-- Elispy")
    print("--  Python to Elisp pretty printer")

    s = "def f(x): return x+1"
    a = ast.parse(s)
    b = Desugar().visit(a)
    t = Elispy().visit(b)
    print(t)

    c = Source(fn="increment (cps)").of(s).nanopass(Desugar).into(Elispy)
    qs, qt = c.transpile()
    print(qs)
    print(qt)
    # print(ast.dump(b))
    # main()


def main():
    """Helper, calls premain(Elispy)."""
    return premain(Elispy)


@cli.command()
@click.argument("filename")
def transpile(filename):
    s = Source().load(filename).into(Elispy)
    qs, qt = s.transpile()
    print(qs)
    print(qt)


# Main

if __name__ == "__main__":
    cli()
