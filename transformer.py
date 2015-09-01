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
