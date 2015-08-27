# Definitions

sfact = '''
def fact(n):
    if (n < 2):
        return 1
    else:
        return n * fact(n - 1)

[fact(5), fact(10)]
'''

swrap = '''
def l(n):
    return [n]
'''

sfor = '''
def c():
    return [x+x+1 for x in [1,2,3,4] if x % 2]
'''

smap = '''
def map(f, l):
    if len(l) == 0:
        return []
    return [f(l[0])] + map(f, l[1:])
'''

ssetcomp = '''
def sc(a,b):
    return {x for x in 
           [e for e in
            range(10) if e > 0 and e < 5
                      if e + 1 < 4]}
'''

sclass = '''
class X:
    x=1
    def wat(self, x):
        return x
'''

slam = '''
lambda x: x
'''

snippets = {
    'setcomp': ssetcomp
    , 'factorial': sfact
    , 'wrap': swrap
    , 'for': sfor
    , 'map': smap
    , 'lambda': slam
    , 'class': sclass
}
