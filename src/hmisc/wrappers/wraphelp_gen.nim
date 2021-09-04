## Generate C and C++ wrappers for nim code

import
  ../core/[all, code_errors],
  std/[macros, options, strutils, strformat, sequtils]

type
  CgenResultKind = enum
    crkProc
    crkType
    crkTypeAlias

  CgenResult = object
    baseName: string
    exportcName: string
    case kind: CgenResultKind
      of crkProc:
        arguments: seq[NimNode]
        returnType: Option[NimNode]
        isMethodOf: bool

      of crkType:
        nested: seq[CgenResult]

      else:
        discard


var
  cgenImpls {.compiletime.}: seq[CgenResult]
  cgenActive {.compiletime.}: bool


macro cgenInit*() =
  if cgenActive:
    error("Cannot init cgen twice - previous module did not call cgen finalizer")

  cgenActive = true

proc convertType(node: NimNode): string =
  case node.kind:
    of nnkIdent:
      result = node.strVal()

    else:
      raise newImplementKindError(node, node.treeRepr())

proc convert(cgen: CgenResult, inType: bool = false): string =
  case cgen.kind:
    of crkType:
      result.add &"struct {cgen.baseName} {{\n"
      for gen in cgen.nested:
        result.add convert(gen, true).indent(4)

      result.add "\n}"

    of crkProc:
      var args: seq[string]
      var callArgs: seq[string]
      for idx, arg in cgen.arguments:
        if not inType or 0 < idx:
          args.add &"{arg[1].convertType()} {arg[0].strVal()}"
          callArgs.add arg[0].strVal()

        elif inType:
          callArgs.add "*this"


      let ret = tern(
        cgen.returnType.isSome(), cgen.returnType.get().convertType(), "void")

      result = fmt"""
{ret} {cgen.baseName}({args.join(",")}) {{
  {cgen.exportcName}({callArgs.join(", ")});
}}
"""

    else:
      discard

macro cgenWriteInCache*(args: varargs[untyped]) =
  var text = ""

  for impl in cgenImpls:
    text.add convert(impl)
    text.add "\n\n\n"

  var final = ""
  for line in text.split('\n'):
    if line.len == 0 or not line.allIt(it in {' '}):
      final.add line
      final.add "\n"

  writeFile("/tmp/res.cpp", final)

  cgenActive = false

proc cgenImpl(impl: NimNode, args: seq[tuple[name, value: NimNode]]): NimNode =
  if not cgenActive:
    error("Must call `cgenInit` for module before using `.cgen.` annotation.", impl)

  case impl.kind:
    of nnkTypeDef:
      var def = CGenResult(kind: crkType, baseName: impl[0][0].strVal())
      cgenImpls.add def

    of nnkProcDef:
      var def = CgenResult(kind: crkProc)
      def.baseName = impl.name.strVal()

      for arg in impl.params()[1..^1]:
        def.arguments.add arg

      if impl.params()[0].kind != nnkEmpty:
        def.returnType = some impl.params[0]

      for (name, value) in args:
        if name.eqIdent("methodof"):
          def.isMethodOf = value.eqIdent("true")

        elif name.eqIdent("mangle"):
          def.exportcName = value.strVal()


      if def.isMethodOf:
        for impl in mitems(cgenImpls):
          if def.arguments[0][1].eqIdent(impl.baseName):
            echov impl.baseName
            impl.nested.add def
            break

      else:
        cgenImpls.add def

    else:
      raise newImplementKindError(impl, impl.treeRepr())

  result = impl

  # var exported =


macro cgen*(args: untyped, impl: untyped): untyped =
  var args1: seq[(NimNode, NimNode)]
  for arg in args:
    args1.add((arg[0], arg[1]))

  result = cgenImpl(impl, args1)

macro cgen*(impl: untyped): untyped =
  result = cgenImpl(impl, @[])



when isMainModule:
  static:
    startHaxComp()

  cgenInit()

  type
    TestClass {.cgen.} = object
      field1*: int

    TestSeq {.cgen.} = seq[int]

  proc meth*(c: TestClass) {.cgen: (methodof: true, mangle: "meth_exported").} =
    echov c.field1

  cgenWriteInCache()
