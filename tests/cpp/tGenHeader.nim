import
  hmisc/preludes/unittest,
  hmisc/wrappers/wraphelp_gen

static:
  startHaxComp()

cgenInit("${cacheDir}/${file}")

type
  TestClass {.cgen.} = object
    field1*: int

  TestSeq {.cgen.} = seq[int]

proc meth*(c: ptr TestClass) {.cgen: (methodof: true).} =
  echov c.field1

cgenWrite()
