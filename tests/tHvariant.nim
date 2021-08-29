import hmisc/preludes/unittest

testFileStarted()


import
  hmisc/types/hvariant,
  hmisc/preludes/unittest


import std/[strutils]

suite "Variant":
  test "Set value":
    var v: Var2[int, float]
    v.setv(12)
    doAssert v.idx == 0

  test "{hasType}":
    var v: Var2[int, float]
    v.setv(1)
    doAssert v.hasType(int)
    v.setv(1.2)
    doAssert v.hasType(float)

  test "{hasType} Sequence of variants":
    var s: seq[Var2[float, string]]
    s.add 1.2
    s.add 2.3
    s.add 9.0

    for it in s:
      doAssert it.hasType(float)

    s.add "Hello"

    doAssert s[^1].hasType(string)

    var b = s
    doAssert b[0].hasType(float)
    doAssert b[^1].hasType(string)

    for it in s:
      if it.hasType(float):
        doAssert it.idx == 0
        discard it.get(float)
      elif it.hasType(string):
        doAssert it.idx == 1
        discard it.get(string)

  test "{get} Compilation errors":
    var v: Var2[float, string]
    doAssert compiles(v.get(float))
    doAssert compiles(v.get(string))
    doAssert not compiles(v.get(int))
    v.setv("hello")
    var raised: bool = false
    try:
      doAssert v.idx == 1
      discard v.get(float)
      fail()
    except:
      doAssert v.get(string) == "hello"
      doAssert getCurrentExceptionMsg().startsWith(
        "Cannot get value for type")
      raised = true

    doAssert raised



  test "{toVar2}":
    block:
      let v = toVar2[int, float](12)

  test "IterTypes":
    for field in fields(typeTuple(toVar2[int, float](12))):
      echo typeof field

    for field in fields(typeTuple(toVar3[int, float, string](12))):
      echo typeof field


testFileEnded()