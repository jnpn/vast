class Dummy(ast.NodeVisitor):

    '''Old useless visitor as grammar learning code.'''

    def visit_Sub(self, s):
        print('SUB')

    def visit_Mult(self, m):
        print('MULT')

    def visit_Num(self, n):
        print(n.n)

    def visit_BinOp(self, b):
        print('{')
        self.visit(b.left)
        self.visit(b.op)
        self.visit(b.right)
        print('}')

    def visit_Lt(self, l):
        print('<')

    def visit_Gt(self, g):
        print('>')

    def visit_FunctionDef(self, f):
        print('LET %s = ' % f.name)
        self.visit(f.args)
        for _ in f.body:
            self.visit(_)

    def visit_Compare(self, c):
        print('(')
        self.visit(c.left)
        for _ in c.ops:
            self.visit(_)
        for _ in c.comparators:
            self.visit(_)
        print(')')

    def visit_Return(self, r):
        print('RET')
        self.visit(r.value)

    def visit_Name(self, n):
        print('$%s' % n.id)

    def visit_Call(self, c):
        print('CALL')
        self.visit(c.func)
        for _ in c.args:
            self.visit(_)

    def visit_If(self, i):
        print('IF')
        self.visit(i.test)
        for _ in i.body:
            self.visit(_)
        print('ORELSE')
        for _ in i.orelse:
            self.visit(_)
