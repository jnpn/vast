from vast.vast import Source
from vast.visitors import visitors as v


def test_source():
    """Test ?."""
    src = """
print(1+2)
"""
    t = Source(src).into(v.Elispy)
    qsource, qtarget = t.transpile()
    expected = """```
## ? (Python|source)

print(1+2)

```"""
    assert qsource == expected
