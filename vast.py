'''
Mostly a Python to (Emacs)Lisp pretty printer.

Many AST visitors have been implemented. The useful one
being Elispy, which has a fallback to generic_visit for
cases not handled yet.
'''

import ast
from subprocess import Popen, PIPE

from visitors import Elispy, Dummy, Generic, Meta
from snippets import snippets

def debug(s):
    '''Parse source then pass the ast to the Dummy visitor.'''
    a = ast.parse(s)
    v = Dummy()
    v.visit(a)

def premain(visitor):
    '''Parse test snippets and pass them to visitor.'''
    for name, source in snippets.items():
        print('##', name.capitalize())
        print('```')
        print(source)
        print('```')
        v = visitor().visit(ast.parse(source))
        print(v)
        print()

def emacs_eval(elisp):
    '''Submit pretty printed src (from the visitor) to `emacs` for evaluation.'''

    assert Popen(['which', 'emacs'], stdout=PIPE, stderr=PIPE).stderr is not b''

    wrapper = '(message "%%S" (progn %s))'
    # http://www.emacswiki.org/emacs/BatchMode
    emacs = Popen(['emacs','-batch','--eval', wrapper % elisp]
                  , stderr=PIPE
                  , stdout=PIPE)
    return emacs.stderr.read().decode('utf8')

def talk(s):
    return list(ast.walk(ast.parse(s)))

def elispy(s):
    ''' parse s then pass it to the Elispy visitor '''
    print(s)
    Elispy().visit(ast.parse(s))

def main():
    '''Helper, calls premain(Elispy).'''
    return premain(Elispy)

# Main

if __name__ == '__main__':

    premain(Elispy)
