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

    def syntax(self, name, sub, beg='<', end='>', pre='meta:'):
        return '%s%s%s %s%s' % (beg, pre, name, sub, end)

    def field(self, name, node, fmt='%s=%s'):
        return fmt % (name, node)

    def meta_visit(self, node):

        def dispatch(node):
            if type(node) is type([]):
                return self.visicat(node, sep=' ')
            else:
                return self.visit(node)

        fields = ast.iter_fields(node)
        vfields = ' '.join(self.field(name, dispatch(node))
                           for name, node in fields)
        return self.syntax(node.__class__.__name__, vfields)

    def generic_visit(self, node):

        if super().atomp(node) or super().nilp(node):
            return node
        elif super().listp(node):
            if len(node) == 1:
                node = node[0]
            elif len(node) == 0:
                print('[error]', 'node is an empty list')
            else:
                print('[warn]', 'node is a list of size > 1, '
                      'elements above 1 are ignored (@TOFIX)')
        else:
            return self.meta_visit(node)

class Flispy(Meta):

    '''
    Thin inherited override of Meta using new methods:
      - syntax
      - field
    in order to get closer to Lisp Meta Language
    '''

    def syntax(self, name, sub):
        return super().syntax(name, sub, pre='', beg='(', end=')')

    def field(self, name, node):
        return super().field('', node, fmt='%s%s')

class Elispy(Meta):

    '''
    Elisp pretty printer (partial), inherits generic printer
    from Meta.
    '''

    def visit_Module(self, m):
        p = ElispyPrelude().emit()
        b = ' '.join([self.visit(_) for _ in m.body])
        return p + b

    def visit_For(self, f):
        t = self.visit(target)
        i = self.visit(i)
        bs = self.visitcat(bs)
        os = self.visit(os)
        return '(each (lambda (%s) %s %s) %s)' % (t, bs, os, i)

    # def visit_ListComp(self, l):
    #     for gen in l.generators:
    #         '''(map (lambda (<gen.bs>) @rec)
    #              (filter (lambda (gen.bs) gen.filter)
    #                  gen.iter))'''

    #     print(list((n, self.visit(e)) for n, e in ast.iter_fields(l)))
    #     return '(@TOFIX filter (...) (map (...) ...))'
    #     return ' '.join([self.visit(node) for name, node in ast.iter_fields(l)])

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

    def visit_Str(self, s):
        return '"%s"' % s.s

    def visit_BinOp(self, b):
        o = self.visit(b.op)
        l = self.visit(b.left)
        r = self.visit(b.right)

        return '(%s %s %s)' % (o, l, r)
    
    def visit_BoolOp(self, b):
        vals = ' '.join([self.visit(v) for v in b.values])
        return '(%s %s)' % (self.visit(b.op), vals)
        
    # cmpop list : Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
    # Boolean operators are simple names, the relationship is made by BoolOp
    def visit_Or(self, o):
        return 'or'

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
        return 'generic-add'

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
        b = '\n    '.join([self.visit(_) for _ in f.body])
        return '(defun %s (%s)\n  (progn\n    %s))' % (f.name, a, b)

    def visit_Lambda(self, l):
        a = self.visit(l.args)
        b = self.visit(l.body)
        return '(lambda (%s) %s)' % (a, b)

    def visit_Attribute(self, a):
        v = self.visit(a.value)
        at = self.visit(a.attr)
        return '(lambda (_) (. %s %s _))' % (v, at)

    def visit_arguments(self, a):
        p = ' '.join([_.arg for _ in a.args]) if a.args else ''
        k = ' '.join([_.arg for _ in a.kwarg]) if a.kwarg else ''

        #     args =     (a,b,c,d,...)
        #     defaults =     (v,w,...)
        # ->  pos,opt = (a,b)(c,d,...)
        pos = a.args[:-len(a.defaults)]
        opt = a.args[-len(a.defaults):]
        #d = '&optional' + ' '.join([self.visit(_) for _ in a.defaults]) if a.defaults else ''
        p = ' '.join([_.arg for _ in pos]) if pos else ''
        d = ('&optional ' + ' '.join([_.arg for _ in opt])) if opt else ''
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
        return '(%s %s)' % (f, a) if a else '(%s)' % f
    
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

    def visit_ClassDef(self, cd):
        '''
        <py>Class(name, [parent], body)
            body ~ inst | FunctionDef
        <el>Defclass(name. [parent], )
        '''
        return '(defclass %s (%s) %s)' % (cd.name, ' '.join(cd.bases), self.visicat(cd.body))

    def visit_Tuple(self, t):
        return '(tuple %s)' % ' '.join(self.visit(e) for e in t.elts)

class ElispyPrelude:

    def emit(self):
        prelude = '''
;;; Elispy Prelude
;;;
(defun generic-add (a b)
  "Generic add of A B."
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((and (floatp a) (floatp b)) (+ a b))
        ((and (stringp a) (stringp b)) (string-join a b))
        (t (error (list a b :type-mismatch)))))

(defun tuple (&rest vs)
  "~Fake multiple return values."
  (apply #'values vs))

;;; Compiled Code

'''
        return prelude
