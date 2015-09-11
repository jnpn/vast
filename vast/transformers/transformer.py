'''
Python ast transformer for Continuation Passing Style programming.

Appends a `continuation` argument to every functions.
'''

import ast


class Desugar(ast.NodeTransformer):

    def visit_arguments(self, a):
        # print('[pre]', ast.dump(a))
        a.args.append(ast.arg(arg='k', annotation=['a -> a']))
        # print('[post]', ast.dump(a))
        return a

    def visit_FunctionDef(self, f):
        nargs = self.visit(f.args)
        # print(ast.dump(f.args), '->', ast.dump(nargs))
        f.args = nargs
        return f

'''
Desugar toward a subset of python closer to Lisp.
So that Visitor can be a simpler morphism.

Python traits missing in Emacs Lisp
- built in types
- default value for keyword arguments
- comprehensions
- decorators
'''


class Defaulted(Desugar):
    '''
    def f(..., k0=d0, ..., kn=dn):
      <body>
    ->
    def f(..., k0, ..., kn):
        # defaulting prologue block
        k0 = k0 or d0
        ...
        kn = kn or dn
        # body

    Note: the FunctionDef.defaults n values maps to
          the n last names of arguments.

    Note: later Elispy can still transform:
          def f(x, y, z=1, t=0)
          to (defun f (x y &optional z t)
               (progn
                 (setq z (or z 1)
                 ...)))

    >>> def kw(a,b,c=1,d=2):
    ...     print(a,b,c,d)
    ... 
    >>> kw(1,2,**{'c':11, 'd':22})
    1 2 11 22
    >>> kw(1,2,**{'c':11, 'd':22, 'e':'e'})
    # TypeError: kw() got an unexpected keyword argument 'e'
    >>> kw(1,2,**{'c':11, 'd':22, 'e':'e'})
    '''
    def visit_FunctionDef(self, fd):

        # import codegen # ?

        def default_bindings(fd):
            return zip(reversed(fd.args.args), reversed(fd.args.defaults))

        def binding_to_assign(arg, val):
            return ast.Assign([ast.Name(arg.arg, ast.Load())], val)

        def assign_or_default(arg, val):
            return ast.Assign([ast.Name(arg.arg, ast.Load())], 
                              ast.BoolOp(ast.Or(), [arg.arg, val]))

        args = fd.args.args
        defaults = fd.args.defaults
        body = fd.body
        prologue = [assign_or_default(arg, val)
                    for arg, val
                    in default_bindings(fd)]

        fd.body = list(reversed(prologue)) + body
        # fd.args.defaults = []
        return fd


class ListComp(Desugar):
    '''see README'''
    def visit_ListComp(self, node):
        pass


class Comp(Desugar):
    '''
    [F gens...] -> LinComp List F <gens>
    {KV gens...} -> Dict LinComp endtype=List KV <gens>
    {G gens...} -> LinComp endtype=Set G <gens>

    make it a super class of ListComp(Desugar) ?
    '''
    pass


class Import(Desugar):
    '''
    ahaha.. ha. hhhu T_T
    '''
    pass

# def f(x):
#     n = 1 + 10 + 100 + y
#     return ">>>" + " " + str(n + x * 3//34)


class Obfuscate(ast.NodeTransformer):

    def names(self, n):
        pass

    def visit_Module(self, m):
        pass

    def visit_FunctionDef(self, fd):
        pass

    def visit_ClassDef(self, cd):
        pass
