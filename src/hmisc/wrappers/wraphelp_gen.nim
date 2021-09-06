## Generate C and C++ wrappers for nim code

import
  ../core/[all, code_errors],
  ../other/oswrap,
  std/[
    macros, options, strutils, strformat, sequtils, strtabs,
    compilesettings, os, parseutils
  ],
  ./wraphelp_store,
  ./wraphelp_decl

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
        constructorArgs: Option[seq[NimNode]]
        constructorOf: Option[string]
        isMethodOf: bool
        isOverride: bool
        isSlot: bool
        isSignal: bool

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
    class: Option[string]

var
  cgenState {.compiletime.}: CgenState

proc isConstructor(cgen: CgenResult): bool =
  cgen.constructorArgs.isSome()

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

        result.add "\n};"

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


        # if inType:
            # if arg[1] of nnkPtrTy:
            #   callArgs.add "this"

            # else:
            #   callArgs.add "*this"


        for idx, arg in cgen.arguments:
          let conv = arg[1].convertType()
          if 0 < idx: decl.add ", "
          decl.add &"{conv} {arg[0].strVal()}"
          args.add &"{conv} {arg[0].strVal()}"
          callArgs.add arg[0].strVal()


        decl.add ");"
        if cgen.isConstructor:
          result.add &"{cgen.exportcName}({args.join(\", \")}) "
          let args = cgen.constructorArgs.get()
          if 0 < args.len:
            result.add ": "
            for arg in args:
              result.add arg.repr()

          result.add " { }"

        else:
          forward.add decl
          let doReturn = tern(cgen.returnType.isSome(), "return ", "")

          result.add &"inline {ret} {cgen.baseName}({args.join(\",\")}) "
          result.add &"{{ {doReturn}{cgen.exportcName}"
          result.add &"({callArgs.join(\", \")}); }}\n"

      else:
        discard

  var forward: seq[string]
  let impl = aux(cgen, false, forward)
  result = forward.join("\n") & "\n\n\n" & impl

macro cgenHeaders*(headers: static[seq[string]]) =
  cgenState.headers.add headers

proc cgenWrite(state: CGenState, impls: seq[CgenResult]): AbsFile =
  var text = ""

  for header in state.headers:
    text.add &"#include {header}\n"

  for impl in impls:
    text.add convert(impl)
    text.add "\n\n\n"

  var final = ""
  for line in text.split('\n'):
    if line.len == 0 or not line.allIt(it in {' '}):
      final.add line
      final.add "\n"

  let path = state.target % state.vars
  echo final
  writeFile(path, final)
  result = AbsFile(path)


proc methodDeclAux(state: CGenState, impl: NimNode): CGenResult =
  result = CgenResult(kind: crkProc)
  result.baseName = impl.name.strVal()

  for arg in impl.params()[1..^1]:
    result.arguments.add arg

  if impl.params()[0].kind != nnkEmpty:
    result.returnType = some impl.params[0]

  for value in impl.pragma:
    if value of nnkIdent:
      case value.strVal():
        of "methodof": result.isMethodOf = true
        of "slot": result.isSlot = true
        of "signal": result.isSignal = true
        of "override": result.isOverride = true
        else:
          raise newImplementKindError(value.strVal())

    else:
      case value[0].strVal():
        of "constructor":
          var args: seq[NimNode]
          for arg in value[1..^1]:
            args.add arg

          result.constructorArgs = some args

        else:
          raise newImplementKindError(value[0].strVal())

  if result.isConstructor:
    result.baseName = "new" & state.class.get()

  let e1 = state.mangle %? newStringTable({"nimName": result.baseName})
  result.exportcName = e1 % state.vars




# proc cgenImpl(impl: NimNode, args: seq[NimNode]): NimNode =
#   if not cgenState.active:
#     error("Must call `cgenInit` for module before using `.cgen.` annotation.", impl)

#   case impl.kind:
#     of nnkTypeDef:
#       # echo impl.treeRepr()
#       var def = CGenResult(kind: crkType, baseName: impl[0][0].strVal())
#       let ofi = impl[^1][1]
#       if ofi.kind == nnkOfInherit:
#         def.parent.add ofi.toSeq()

#       cgenState.impls.add def

proc splitClass(class: NimNode): tuple[name: NimNode, super: seq[NimNode]] =
  case class.kind:
    of nnkIdent:
      result.name = class

    of nnkInfix:
      result.name = class[1]
      result.super.add class[2]

    else:
      raise newImplementKindError(class)

proc toCxx*(res: CgenResult, header: CxxHeader): CxxEntry =
  case res.kind:
    of crkType:
      var obj = initCxxObject(res.baseName, res.exportcName)
      obj.icpp = res.exportcName

      for meth in res.nested:
        obj.nested.add toCxx(meth, header)

      result = box(obj)

    of crkProc:
      var pr = initCxxProc(res.baseName, res.exportcName)

      for arg in res.arguments:
        pr.add initCxxArg(arg[0].strVal(), arg[1].cxxTypeAux())

      pr.constructorOf = res.constructorOf

      result = box(pr)


    of crkTypeAlias:
      discard

  result.setHeaderRec(header)

macro cgen*(outfile: static[string], args: varargs[untyped]): untyped =
  result = newStmtList()

  var state: CGenState
  state.target = outfile
  state.mangle = "${filename}_${nimName}"

  let (dir, file, ext) = splitFile(args.lineInfoObj().filename)
  state.vars = newStringTable({
    "cacheDir": querySetting(nimcacheDir),
    "sourceDir": dir,
    "filename": file,
    "file": file & ".hpp"
  }, modeCaseInsensitive)

  var impls: seq[CGenResult]
  for entry in args[^1]:
    if entry of nnkIncludeStmt:
      state.headers.add entry[0].strVal()

    elif entry of nnkCommand and entry[0].eqIdent("class"):
      let (name, super) = splitClass(entry[1])
      var class = CGenResult(
        kind: crkType, parent: super,
        baseName: name.strVal(), exportcName: name.strVal())

      state.class = some class.baseName
      for meth in entry[^1]:
        class.nested.add state.methodDeclAux(meth)
        if class.nested.last().isConstructor:
          class.nested.last().exportcName = class.exportcName

      state.class = none string

      impls.add class

  let header = initCxxHeader state.cgenWrite(impls)


  result = impls.mapIt(toCxx(it, header)).toNNode()
  # for entry in impls:
  #   result.add toCxxEntry(entry, header).toNNode()

  echo result.repr()
