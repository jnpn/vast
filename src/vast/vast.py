import ast
from subprocess import PIPE, Popen

from .visitors.visitors import Elispy, Generic


class Source:

    """
    Packs source related methods into a single unit.
    """

    def __init__(self, source="None", fn="?", visitor=Generic):
        self.transformer = None
        self.visitor = visitor
        self.source = source
        self.fn = fn

    def load(self, fn):
        with open(fn, encoding="utf8") as src:
            self.source = "".join(src.readlines())
        return self

    def of(self, source):
        self.source = source
        return self

    def nanopass(self, transformer):
        self.transformer = transformer
        return self

    def into(self, visitor):
        self.visitor = visitor
        return self

    def __repr__(self):
        return f"<Source {self.source}>"

    def talk(self):
        return list(ast.walk(ast.parse(self.source)))

    def elispy(self):
        """Parse source then pass the ast to the Elispy visitor."""
        a = ast.parse(self.source)
        return Elispy().visit(a)

    def debug(self):
        """Parse source then pass the ast to the Generic visitor."""
        a = ast.parse(self.source)
        return Generic().visit(a)

    def quotecode(self, code):
        return f"```\n{code}\n```"

    def emacs_eval(self):
        """
        Submit pretty printed src (from the visitor)
        to `emacs` for evaluation.
        """

        with Popen(["which", "emacs"], stdout=PIPE, stderr=PIPE) as which:
            assert which.stderr != b""

        wrapper = '(message "%%S" (progn %s))'
        # http://www.emacswiki.org/emacs/BatchMode
        code = self.visitor().visit(ast.parse(self.source))
        cmd = ["emacs", "-batch", "--eval", wrapper % code]
        with Popen(cmd, stderr=PIPE, stdout=PIPE) as emacs:
            return emacs.stderr.read().decode("utf8")

    def transpile(self):
        qs = self.quotecode(f"## {self.fn} (Python|source)\n" + self.source)
        ast0 = ast.parse(self.source)
        print("[transpile][pre]", ast.dump(ast0))
        ast1 = ast0 if not self.transformer else self.transformer().visit(ast0)
        print("[transpile][post]", ast.dump(ast1))
        tgt = self.visitor().visit(ast1)
        qt = self.quotecode(f";; {self.fn} (Lisp|target)\n\n{tgt}\n")
        return qs, qt
