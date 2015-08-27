import ast

class Generic(ast.NodeVisitor):

    '''
    Traditional AST recursive traversal `a la` Lisp
    With crude identation. Not a visitor actually.
    '''

    def children(self, node):
        return [node.__getattribute__(f) for f in node._fields]

    @staticmethod
    def listp(x):
        return type(x) is type([])

    @staticmethod
    def atomp(x):
        return type(x) is type('') or type(x) is type(0)

    @staticmethod
    def nilp(x):
        return type(x) is type(None)

    def rec_visit(self, node, ind=0, ins='-', inc=2, pre='`'):

        print(pre + (ins * ind), node.__class__.__name__)
        for _ in self.children(node):
            if Generic.listp(_):
                for __ in _:
                    self.rec_visit(__, ind = ind + inc)
            elif Generic.atomp(_):
                print(pre + (ins * ind) , '%s:%s' % (_, type(_).__name__))
            elif Generic.nilp(_):
                print(pre + (ins * ind), '.') #'ℵ')
            else:
                self.rec_visit(_, ind = ind + inc)

    def generic_visit(self, node):
        return self.rec_visit(node)

class Meta(Generic):

    '''
    Meta visitor, ugly variant of Generic, but uses the official
    generic_visit from the API.
    '''

    def debug(self, node):
        '''Debug helper, ~unsound code.'''
        nodename = node.__class__.__name__
        visitorname = self.__class__.__name__
        nodefields = list(ast.iter_fields(node))
        node_ = list(ast.iter_child_nodes(node))
        print('[warn]', nodename, nodefields, node_)
        print('[warn]', '<visit_%s not implemented in %s>' \
              % (nodename, visitorname))

    def visicat(self, subs, sep='.'):
        '''Mapconcat self.visit over [astnodes].'''
        return sep.join([self.visit(sub) for sub in (subs or [])])

    def meta_visit(self, node):

        def dispatch(node):
            if type(node) is type([]):
                return self.visicat(node, sep=' ')
            else:
                return self.visit(node)

        fmt = '%s=%s'
        fields = ast.iter_fields(node)
        vfields = ' '.join(fmt % (name, dispatch(node)) for name, node in fields)
        return '<meta:%s %s>' % (node.__class__.__name__, vfields)

    def generic_visit(self, node):

        if super().atomp(node) or super().nilp(node):
            return node
        elif super().listp(node):
            if len(node) == 1:
                node = node[0]
            elif len(node) == 0:
                print('[error]', 'node is an empty list')
            else:
                print('[warn]', 'node is a list of size > 1, elements above 1 are ignored (@TOFIX)')
        else:
            return self.meta_visit(node)
    
class Elispy(Meta):

    '''
    Elisp pretty printer (partial), inherits generic printer
    from Meta.
    '''

    def visit_Module(self, m):
        b = ' '.join([self.visit(_) for _ in m.body])
        return b

    def visit_For(self, f):
        t = self.visit(target)
        i = self.visit(i)
        bs = self.visitcat(bs)
        os = self.visit(os)
        return '(each (lambda (%s) %s %s) %s)' % (t, bs, os, i)

    def visit_ListComp(self, l):
        # print(list((n, self.visit(e)) for n, e in ast.iter_fields(l)))
        # return '(@TOFIX filter (...) (map (...) ...))'
        return ' '.join([self.visit(node) for name, node in ast.iter_fields(l)])

    def visit_Comprehension(self, c):
        t = self.visit(c.target)
        i = self.visit(c.iter)
        ifs = self.visicat(c.ifs)
        return '(map (lambda (%s) %s) %s)' % (t, i, ifs)

    def visit_List(self, l):
        e = self.visicat(l.elts, sep=' ')
        return '(list %s)' % e

    def visit_Sub(self, s):
        return '-'

    def visit_Mult(self, m):
        return '*'

    def visit_Num(self, n):
        return str(n.n)

    def visit_BinOp(self, b):
        o = self.visit(b.op)
        l = self.visit(b.left)
        r = self.visit(b.right)

        return '(%s %s %s)' % (o, l, r)
    
    # def visit_BoolOp(self, b):
    #     return self.meta_visit(b)
        
    # cmpop list : Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

    def visit_Lt(self, l):
        return '<'

    def visit_Gt(self, g):
        return '>'

    def visit_Eq(self, e):
        return 'equal'

    def visit_NotEq(self, n):
        return '(neg equal)'

    # TODO : missing cmpops

    # operator list : operator = Add | Sub | Mult | Div | Mod | Pow | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv

    def visit_Add(self, a):
        return '(+)'

    # TODO : missing operators

    def visit_And(self, a):
        return 'and'

    def visit_Is(self, a):
        return 'eq'

    def visit_Subscript(self, s):
        v = self.visit(s.value)
        s = self.visit(s.slice)
        # c = self.visit(s.ctx)
        return '(sub %s %s)' % (v, s)

    def visit_Index(self, i):
        n = self.visit(i.value)
        return n

    def visit_Slice(self, s):
        l = self.visit(s.lower) if s.lower else ''
        u = self.visit(s.upper) if s.upper else ''
        s = self.visit(s.step) if s.step else ''
        # rewrite as if-concat
        return ' '.join([_ for _ in [l, u, s] if _ is not ''])

    def visit_FunctionDef(self, f):
        a = self.visit(f.args)
        b = ' '.join([self.visit(_) for _ in f.body])
        return '(defun %s (%s) %s)' % (f.name, a, b)

    def visit_Lambda(self, l):
        a = self.visit(l.args)
        b = self.visit(l.body)
        return '(lambda (%s) %s)' % (a, b)

    def visit_arguments(self, a):
        p = ' '.join([_.arg for _ in a.args]) if a.args else ''
        k = ' '.join([_.arg for _ in a.kwarg]) if a.kwarg else ''
        d = ' '.join([_.arg for _ in a.defaults]) if a.defaults else ''
        return ('%s %s %s' % (p, k, d)).strip()

    def visit_Compare(self, c):
        l = self.visit(c.left)
        o = ' '.join([self.visit(_) for _ in c.ops]) if c.ops else ''
        c = ' '.join([self.visit(_) for _ in c.comparators]) if c.comparators else ''
        return '(%s %s %s)' % (o, l, c)

    def visit_Return(self, r):
        r = self.visit(r.value)
        return r

    def visit_Expr(self, e):
        v = self.visit(e.value)
        return v

    def visit_Name(self, n):
        return str(n.id)

    def visit_NameConstant(self, nc):
        return str(nc.value)

    def visit_Call(self, c):
        f = self.visit(c.func)
        a = self.visicat(c.args, sep=' ')
        return '(%s %s)' % (f, a)
    
    def visit_If(self, i):
        t = self.visit(i.test)
        # b = ' '.join([self.visit(_) for _ in i.body])
        # o = ' '.join([self.visit(_) for _ in i.orelse])
        b = self.visicat(i.body)
        o = self.visicat(i.orelse)
        return '(if %s %s %s)' % (t, b, o)

    def visit_Assign(self, a):
        tgs = self.visicat(a.targets)
        val = self.visit(a.value)
        return '(setq %s %s)' % (tgs, val)

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
