## Generate C and C++ wrappers for nim code

import
  ../core/[all, code_errors],
  std/[
    macros, options, strutils, strformat, sequtils, strtabs,
    compilesettings, os, parseutils
  ]

func `%?`(str: string, table: StringTableRef): string =
  for (kind, value) in interpolatedFragments(str):
    if kind in {ikVar, ikExpr} and value in table:
      result.add table[value]

    else:
      case kind:
        of ikExpr:
          result.add "${"
          result.add value
          result.add "}"

        of ikVar:
          result.add "$"
          result.add value

        of ikDollar:
          result.add "$$"

        else:
          result.add value

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
        parent: seq[NimNode]
        nested: seq[CgenResult]

      else:
        discard

  CgenState = object
    impls: seq[CgenResult]
    active: bool
    target: string
    vars: StringTableRef
    mangle: string
    headers: seq[string]

var
  cgenState {.compiletime.}: CgenState


macro cgenInit*(
    target: string{nkStrLit},
    mangle: static[string] = "${filename}_${nimName}"
  ) =

  if cgenState.active:
    error("Cannot init cgen twice - previous module did not call cgen finalizer")

  cgenState.target = target.strVal()
  cgenState.mangle = mangle

  let (dir, file, ext) = splitFile(target.lineInfoObj().filename)
  cgenState.vars = newStringTable({
    "cacheDir": querySetting(nimcacheDir),
    "sourceDir": dir,
    "filename": file,
    "file": file & ".hpp"
  }, modeCaseInsensitive)

  cgenState.active = true

proc skipTy(t: NimNode): NimNode =
  result = t
  while result of {nnkRefTy, nnkPtrTy}:
    result = result[0]

proc convertType(node: NimNode): string =
  case node.kind:
    of nnkIdent:
      result = node.strVal()

    of nnkPtrTy:
      result = convertType(node[0]) & "*"

    else:
      raise newImplementKindError(node, node.treeRepr())

proc convert(cgen: CgenResult): string =
  proc aux(
      cgen: CgenResult, inType: bool, forward: var seq[string]): string =
    case cgen.kind:
      of crkType:
        if cgen.nested.len > 0:
          forward.add &"struct {cgen.baseName};"

        var deriveFrom = ""
        for idx, parent in cgen.parent:
          deriveFrom.add tern(idx == 0, ": ", ", ")
          deriveFrom.add "public " & parent.repr()

        result.add &"struct {cgen.baseName}{deriveFrom} {{\n"
        for gen in cgen.nested:
          result.add aux(gen, true, forward).indent(4)

        result.add "\n}"

      of crkProc:
        var args: seq[string]
        var callArgs: seq[string]
        var decl: string
        let ret =
          if cgen.returnType.isSome():
            cgen.returnType.get().convertType()

          else:
            "void"

        decl.add ret
        decl.add &" {cgen.exportcName}("


        for idx, arg in cgen.arguments:
          let conv = arg[1].convertType()
          if 0 < idx:
            decl.add ", "

          decl.add &"{conv} {arg[0].strVal()}"

          if not inType or 0 < idx:
            args.add &"{conv} {arg[0].strVal()}"
            callArgs.add arg[0].strVal()

          elif inType:
            if arg[1] of nnkPtrTy:
              callArgs.add "this"

            else:
              callArgs.add "*this"

        decl.add ");"

        forward.add decl

        let doReturn = tern(cgen.returnType.isSome(), "return ", "")

        result.add &"inline {ret} {cgen.baseName}({args.join(\",\")}) "
        result.add &"{{ {doReturn}{cgen.exportcName}"
        result.add &"({callArgs.join(\", \")}); }}"

      else:
        discard

  var forward: seq[string]
  let impl = aux(cgen, false, forward)
  result = forward.join("\n") & "\n\n\n" & impl

macro cgenHeaders*(headers: static[seq[string]]) =
  cgenState.headers.add headers

macro cgenWrite*() =
  var text = ""

  for header in cgenState.headers:
    text.add &"#include {header}\n"

  for impl in cgenState.impls:
    text.add convert(impl)
    text.add "\n\n\n"

  var final = ""
  for line in text.split('\n'):
    if line.len == 0 or not line.allIt(it in {' '}):
      final.add line
      final.add "\n"

  let path = cgenState.target % cgenState.vars
  echo final
  writeFile(path, final)

  cgenState.active = false


proc cgenImpl(impl: NimNode, args: seq[NimNode]): NimNode =
  if not cgenState.active:
    error("Must call `cgenInit` for module before using `.cgen.` annotation.", impl)

  case impl.kind:
    of nnkTypeDef:
      # echo impl.treeRepr()
      var def = CGenResult(kind: crkType, baseName: impl[0][0].strVal())
      let ofi = impl[^1][1]
      if ofi.kind == nnkOfInherit:
        def.parent.add ofi.toSeq()

      cgenState.impls.add def

    of nnkProcDef:
      var def = CgenResult(kind: crkProc)
      def.baseName = impl.name.strVal()

      for arg in impl.params()[1..^1]:
        def.arguments.add arg

      if impl.params()[0].kind != nnkEmpty:
        def.returnType = some impl.params[0]

      for value in args:
        if value.eqIdent("methodof"): def.isMethodOf = true

      let e1 = cgenState.mangle %? newStringTable({"nimName": def.baseName})
      def.exportcName = e1 % cgenState.vars

      if def.isMethodOf:
        for impl in mitems(cgenState.impls):
          if def.arguments[0][1].skipTy().eqIdent(impl.baseName):
            echov impl.baseName
            impl.nested.add def
            break

      else:
        cgenState.impls.add def

    else:
      raise newImplementKindError(impl, impl.treeRepr())

  result = impl


macro cgen*(args: untyped, impl: untyped): untyped =
  result = cgenImpl(impl, args.toSeq())

macro cgen*(impl: untyped): untyped =
  result = cgenImpl(impl, @[])
