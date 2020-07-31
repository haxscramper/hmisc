import hmisc/types/hvariant

import unittest, strutils

suite "Variant":
  test "Set value":
    var v: Var2[int, float]
    v.setv(12)
    assert v.idx == 0

  test "{hasType}":
    var v: Var2[int, float]
    v.setv(1)
    assert v.hasType(int)
    v.setv(1.2)
    assert v.hasType(float)

  test "{hasType} Sequence of variants":
    var s: seq[Var2[float, string]]
    s.add 1.2
    s.add 2.3
    s.add 9.0

    for it in s:
      assert it.hasType(float)

    s.add "Hello"

    assert s[^1].hasType(string)

    var b = s
    assert b[0].hasType(float)
    assert b[^1].hasType(string)

    for it in s:
      if it.hasType(float):
        assert it.idx == 0
        discard it.get(float)
      elif it.hasType(string):
        assert it.idx == 1
        discard it.get(string)

  test "{get} Compilation errors":
    var v: Var2[float, string]
    assert compiles(v.get(float))
    assert compiles(v.get(string))
    assert not compiles(v.get(int))
    v.setv("hello")
    var raised: bool = false
    try:
      assert v.idx == 1
      discard v.get(float)
      fail()
    except:
      assert v.get(string) == "hello"
      assert getCurrentExceptionMsg().startsWith(
        "Cannot get value for type")
      raised = true

    assert raised



  test "{toVar2}":
    block:
      let v = toVar2[int, float](12)
