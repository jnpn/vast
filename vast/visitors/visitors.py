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

# module Python version "$Revision$"
# {

#       mod = Module(stmt* body)

    def visit_Module(self, m):
        p = ElispyPrelude().emit()
        b = ' '.join([self.visit(_) for _ in m.body])
        return p + b

#           | Interactive(stmt* body)
#           | Expression(expr body)

#           -- not really an actual node but useful in Jython's typesystem.
#           | Suite(stmt* body)

#       stmt = FunctionDef(identifier name, arguments args,
#                             stmt* body, expr* decorator_list)

    def visit_FunctionDef(self, f):
        a = self.visit(f.args)
        if len(f.body) <= 1:
            b = '\n    '.join([self.visit(_) for _ in f.body])
            return '(defun %s (%s)\n  %s)' % (f.name, a, b)
        else:
            b = '\n    '.join([self.visit(_) for _ in f.body])
            return '(defun %s (%s)\n  (progn\n    %s))' % (f.name, a, b)

#             | ClassDef(identifier name, expr* bases, stmt* body, expr* decorator_list)

    def visit_ClassDef(self, cd):
        '''
        <py>Class(name, [parent], body)
            body ~ inst | FunctionDef
        <el>Defclass(name. [parent], )
        '''
        return '(defclass %s (%s) %s)' % (cd.name, ' '.join(b.id for b in cd.bases), self.visicat(cd.body))

#             | Return(expr? value)

    def visit_Return(self, r):
        r = self.visit(r.value)
        return r


#             | Delete(expr* targets)
#             | Assign(expr* targets, expr value)

    def visit_Assign(self, a):
        tgs = self.visicat(a.targets)
        val = self.visit(a.value)
        return '(setq %s %s)' % (tgs, val)

#             | AugAssign(expr target, operator op, expr value)

#             -- not sure if bool is allowed, can always use int
#             | Print(expr? dest, expr* values, bool nl)

#             -- use 'orelse' because else is a keyword in target languages
#             | For(expr target, expr iter, stmt* body, stmt* orelse)


    def visit_For(self, f):
        t = self.visit(f.target)
        i = self.visit(f.iter)
        bs = self.visicat(f.body)
        os = self.visit(f.orelse)
        return '(each (lambda (%s) %s %s) %s)' % (t, bs, os, i)

#             | While(expr test, stmt* body, stmt* orelse)
#             | If(expr test, stmt* body, stmt* orelse)

    def visit_If(self, i):
        t = self.visit(i.test)
        b = self.visicat(i.body)
        o = self.visicat(i.orelse)
        return '(if %s %s %s)' % (t, b, o)

#             | With(expr context_expr, expr? optional_vars, stmt* body)

#             -- 'type' is a bad name
#             | Raise(expr? type, expr? inst, expr? tback)
#             | TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
#             | TryFinally(stmt* body, stmt* finalbody)
#             | Assert(expr test, expr? msg)

#             | Import(alias* names)

    def visit_Import(self, i):
        req = lambda n: '(require \'%s :as \'%s)' % (n.name, n.asname) \
            if n.asname \
            else '(require \'%s)' % n.name
        return '\n'.join(req(n) for n in  i.names)

#             | ImportFrom(identifier? module, alias* names, int? level)

#             -- Doesn't capture requirement that locals must be
#             -- defined if globals is
#             -- still supports use as a function!
#             | Exec(expr body, expr? globals, expr? locals)

#             | Global(identifier* names)
#             | Expr(expr value)

    def visit_Expr(self, e):
        v = self.visit(e.value)
        return v

#             | Pass | Break | Continue

    def visit_Pass(self, p):
        return '(progn)'

#             -- XXX Jython will be different
#             -- col_offset is the byte offset in the utf8 string the parser uses
#             attributes (int lineno, int col_offset)

#             -- BoolOp() can use left & right?
#       expr = BoolOp(boolop op, expr* values)

    def visit_BoolOp(self, b):
        vals = ' '.join([self.visit(v) for v in b.values])
        return '(%s %s)' % (self.visit(b.op), vals)

#            | BinOp(expr left, operator op, expr right)

    def visit_BinOp(self, b):
        o = self.visit(b.op)
        l = self.visit(b.left)
        r = self.visit(b.right)
        return '(%s %s %s)' % (o, l, r)

#            | UnaryOp(unaryop op, expr operand)

    def visit_UnaryOp(self, b):
        o = self.visit(b.op)
        a = self.visit(b.operand)
        return '(%s %s)' % (o, a)

#            | Lambda(arguments args, expr body)

    def visit_Lambda(self, l):
        a = self.visit(l.args)
        b = self.visit(l.body)
        return '(lambda (%s) %s)' % (a, b)

#            | IfExp(expr test, expr body, expr orelse)

    def visit_IfExp(self, i):
        import pdb; pdb.set_trace()
        t = self.visit(i.test)
        b = self.visit(i.body)
        o = self.visit(i.orelse)
        return '(if %s %s %s)' % (t, b, o)

#            | Dict(expr* keys, expr* values)

    def visit_Dict(self, d):
        ks = ' '.join(self.visit(k) for k in d.keys)
        vs = ' '.join(self.visit(v) for v in d.values)
        return '(dict %s %s)' % (ks, vs)

#            | Set(expr* elts)

    def visit_Set(self, s):
        print(s._fields)
        return '(Set %s)' % ' '.join(self.visit(e) for e in s.elts)

#            | ListComp(expr elt, comprehension* generators)
#            | SetComp(expr elt, comprehension* generators)
#            | DictComp(expr key, expr value, comprehension* generators)

#            | GeneratorExp(expr elt, comprehension* generators)
#            -- the grammar constrains where yield expressions can occur
#            | Yield(expr? value)
#            -- need sequences for compare to distinguish between
#            -- x < 4 < 3 and (x < 4) < 3
#            | Compare(expr left, cmpop* ops, expr* comparators)

    def visit_Compare(self, c):
        l = self.visit(c.left)
        o = ' '.join([self.visit(_) for _ in c.ops]) if c.ops else ''
        c = ' '.join([self.visit(_) for _ in c.comparators]) if c.comparators else ''
        return '(%s %s %s)' % (o, l, c)

#            | Call(expr func, expr* args, keyword* keywords,
#                        expr? starargs, expr? kwargs)

    def visit_Call(self, c):
        f = self.visit(c.func)
        a = self.visicat(c.args, sep=' ')
        return '(%s %s)' % (f, a) if a else '(%s)' % f

#            | Repr(expr value)
#            | Num(object n) -- a number as a PyObject.

    def visit_Num(self, n):
        return str(n.n)

#            | Str(string s) -- need to specify raw, unicode, etc?
#            -- other literals? bools?

    def visit_Str(self, s):
        return '"%s"' % s.s

#            -- the following expression can appear in assignment context
#            | Attribute(expr value, identifier attr, expr_context ctx)

    def visit_Attribute(self, a):
        v = self.visit(a.value)
        at = self.visit(a.attr)
        # return '(lambda (_) (. %s %s _))' % (v, at)
        return '(. %s %s)' % (v, at)

#            | Subscript(expr value, slice slice, expr_context ctx)

    def visit_Subscript(self, s):
        v = self.visit(s.value)
        s = self.visit(s.slice)
        # c = self.visit(s.ctx)
        return '(sub %s %s)' % (v, s)

#            | Name(identifier id, expr_context ctx)

    def visit_Name(self, n):
        return str(n.id)

    def visit_NameConstant(self, nc):
        return str(nc.value)

#            | List(expr* elts, expr_context ctx)

    def visit_List(self, l):
        e = self.visicat(l.elts, sep=' ')
        return '(list %s)' % e

#            | Tuple(expr* elts, expr_context ctx)

    def visit_Tuple(self, t):
        return '(tuple %s)' % ' '.join(self.visit(e) for e in t.elts)

#             -- col_offset is the byte offset in the utf8 string the parser uses
#             attributes (int lineno, int col_offset)

#       expr_context = Load | Store | Del | AugLoad | AugStore | Param

#       slice = Ellipsis

    def visit_Ellipsis(self, e):
        return 'core.ellipsis'

#  | Slice(expr? lower, expr? upper, expr? step)

    def visit_Slice(self, s):
        l = self.visit(s.lower) if s.lower else ''
        u = self.visit(s.upper) if s.upper else ''
        s = self.visit(s.step) if s.step else ''
        # rewrite as if-concat
        return ' '.join([_ for _ in [l, u, s] if _ is not ''])

#             | ExtSlice(slice* dims)
#             | Index(expr value)

    def visit_Index(self, i):
        n = self.visit(i.value)
        return n

#       boolop = And | Or

    def visit_And(self, a):
        return 'and'

    def visit_Or(self, o):
        return 'or'

    # cmpop list : Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
    # Boolean operators are simple names, the relationship is made by BoolOp

    def visit_Eq(self, e):
        return 'equal'

    def visit_NotEq(self, n):
        return '(neg equal)'

    def visit_Lt(self, l):
        return '<'

    def visit_Gt(self, g):
        return '>'

    def visit_Is(self, a):
        return 'eq'

    # operator list : operator = Add | Sub | Mult | Div | Mod | Pow | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv

    def visit_Add(self, a):
        return 'generic-add'

    def visit_Sub(self, s):
        return '-'

    def visit_Mult(self, m):
        return '*'

#       operator = Add | Sub | Mult | Div | Mod | Pow | LShift
#                  | RShift | BitOr | BitXor | BitAnd | FloorDiv

#       unaryop = Invert | Not | UAdd | USub

#       cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

#       comprehension = (expr target, expr iter, expr* ifs)

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

#       -- not sure what to call the first argument for raise and except
#       excepthandler = ExceptHandler(expr? type, expr? name, stmt* body)
#                       attributes (int lineno, int col_offset)

#       arguments = (expr* args, identifier? vararg,
#                    identifier? kwarg, expr* defaults)

    def visit_arguments(self, a):
        p = ' '.join([_.arg for _ in a.args]) if a.args else ''
        k = ' '.join([_.arg for _ in a.kwarg]) if a.kwarg else ''
        v = f"&rest {a.vararg.arg}" if a.vararg.arg else ''
        if len(a.defaults):
            #     args =     (a,b,c,d,...)
            #     defaults =     (v,w,...)
            # ->  pos,opt = (a,b)(c,d,...)
            l = len(a.defaults)
            pos = a.args[:-l]
            opt = a.args[-l:]
            p = ' '.join([_.arg for _ in pos]) if pos else ''
            d = ('&optional ' + ' '.join([_.arg for _ in opt])) if opt else ''
            return ('%s %s %s %s' % (p, k, d, v)).strip()
        else:
            return ('%s %s %s' % (p, k, v)).strip()

#         -- keyword arguments supplied to call
#         keyword = (identifier arg, expr value)

#         -- import name with optional 'as' alias.
#         alias = (identifier name, identifier? asname)
# }


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

(defclass PyObject ()
  (add ...)
  (sub ...)
  (in ...)
  (str ...)
  (and ...))

(defclass num (PyObject)
  (str "num"))
(defclass str (PyObject)
  (str "str"))
(defclass float (PyObject)
  (str "float"))

(setq None nil)

(defun print (&rest args)
  (loop for a in args
       do (message "%s" a)))

(defun map (f l) (mapcar f l))

(defun each (f l)
  (loop for e in l
        do (funcall f e)))

(defun tuple (&rest vs)
  "~Fake multiple return values."
  (apply #'values vs))

;;; Compiled Code

'''
        return prelude
