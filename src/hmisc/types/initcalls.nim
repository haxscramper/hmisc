import macros, tables, sets, typetraits, sequtils, strformat, strutils,
       options, parseutils

import ../algo/halgorithm
import ../types/colorstring

func toBracket*(elems: seq[NimNode]): NimNode =
  nnkBracket.newTree(elems)

func toBracketSeq*(elems: seq[NimNode]): NimNode =
  nnkPrefix.newTree(ident "@", nnkBracket.newTree(elems))

type
  NType* = object
    head*: string
    genParams*: seq[NType]

  NVarDeclKind* = enum
    nvdVar
    nvdConst
    nvdLet

  NIdentDefs* = object
    varname: string
    kind: NVarDeclKind
    vtype: NType

func isEnum*(en: NimNode): bool = en.getTypeImpl().kind == nnkEnumTy

func getEnumPref*(en: NimNode): string =
  let
    impl = en.getTypeImpl()
    name = impl[1].strVal()
    pref = name.parseUntil(result, {'A' .. 'Z', '0' .. '9'})


func `$`*(nt: NType): string =
  if nt.genParams.len > 0:
    nt.head & "[" & nt.genParams.mapIt($it).join(", ") & "]"
  else:
    nt.head


func mkNType*(name: string, gparams: seq[string] = @[]): NType =
  NType(head: name, genParams: gparams.mapIt(mkNType(it, @[])))

func mkNType*(name: string, gparams: openarray[NType]): NType =
  NType(head: name, genParams: toSeq(gparams))

func mkNType*(impl: NimNode): NType =
  case impl.kind:
    of nnkBracketExpr:
      mkNType(impl[0].strVal(), impl[1..^1].mapIt(it.mkNType()))
    of nnkIdent, nnkSym:
      mkNType(impl.strVal)
    else:
      raiseAssert("#[ IMPLEMENT ]#")

func toNimNode*(ntype: NType): NimNode =
  if ntype.genParams.len == 0:
    return ident(ntype.head)
  else:
    result = nnkBracketExpr.newTree(newIdentNode(ntype.head))
    for param in ntype.genParams:
      result.add param.toNimNode()

func mkVarDecl*(name: string, vtype: NType,
                kind: NVarDeclKind = nvdLet): NIdentDefs =
  NIdentDefs(varname: name, kind: kind, vtype: vtype)

func toFormalParam*(nident: NIdentDefs): NimNode =
  let typespec =
    case nident.kind:
      of nvdVar: newTree(nnkVarTy, nident.vtype.toNimNode())
      of nvdLet: nident.vtype.toNimNode()
      of nvdConst: newTree(nnkConstTy, nident.vtype.toNimNode())

  nnkIdentDefs.newTree(
    newIdentNode(nident.varname),
    typespec,
    newEmptyNode()
  )

func mkVarDeclNode*(name: string, vtype: NType,
                    kind: NVarDeclKind = nvdLet): NimNode =
  mkVarDecl(name, vtype, kind).toFormalParam()


func mkNTypeNode*(name: string, gparams: seq[string]): NimNode =
  mkNType(name, gparams).toNimNode()

func mkNTypeNode*(name: string, gparams: varargs[NType]): NimNode =
  mkNType(name, gparams).toNimNode()

func mkCallNode*(
  dotHead: NimNode, name: string,
  args: seq[NimNode], genParams: seq[NType] = @[]): NimNode =

  let dotexpr = nnkDotExpr.newTree(dotHead, ident(name))
  if genParams.len > 0:
    result = nnkCall.newTree()
    result.add nnkBracketExpr.newTree(
      @[ dotexpr ] & genParams.mapIt(it.toNimNode))
  else:
    result = nnkCall.newTree(dotexpr)

  for arg in args:
    result.add arg

  # debugecho "\e[31m32333\e[39m"
  # debugecho result.toStrLit().strVal()
  # debugecho result.treeRepr()
  # debugecho "\e[31m32333\e[39m"

func mkCallNode*(
  name: string,
  args: seq[NimNode],
  genParams: seq[NType] = @[]): NimNode =
  if genParams.len > 0:
    result = nnkCall.newTree()
    result.add nnkBracketExpr.newTree(
      @[ newIdentNode(name) ] & genParams.mapIt(it.toNimNode()))

  else:
    result = nnkCall.newTree(ident name)

  for node in args:
    result.add node


func mkCallNode*(name: string,
                 gentypes: openarray[NType],
                 args: varargs[NimNode]): NimNode =
  mkCallNode(name, toSeq(args), toSeq(genTypes))

func mkCallNode*(
  arg: NimNode, name: string,
  gentypes: openarray[NType] = @[]): NimNode =
  mkCallNode(name, @[arg], toSeq(genTypes))

func mkCallNode*(
  dotHead: NimNode, name: string,
  gentypes: openarray[NType],
  args: seq[NimNode]): NimNode =
  mkCallNode(dotHead, name, toSeq(args), toSeq(genTypes))


func toNTypeAst*[T](): NType =
  let str = $typeof(T)
  let expr = parseExpr(str)

func makeInitCalls*[T](val: T): NimNode =
  when T is enum:
    ident($val)
  else:
    newLit(val)

func makeInitAllFields*[T](val: T): NimNode =
  result = newCall("init" & $typeof(T))
  for name, val in fieldPairs(val):
    result.add nnkExprEqExpr.newTree(
      ident(name), makeInitCalls(val))

func makeInitCalls*[A, B](table: Table[A, B]): NimNode =
  mixin makeInitCalls
  result = nnkTableConstr.newTree()
  for key, val in table:
    result.add newColonExpr(key.makeInitCalls, val.makeInitCalls)

  result = newCall(
    nnkBracketExpr.newTree(
      ident("toTable"),
      parseExpr($typeof(A)),
      parseExpr($typeof(B))
    ),
    result
  )

func makeInitCalls*[A](hset: HashSet[A]): NimNode =
  mixin makeInitCalls
  result = nnkBracket.newTree()
  for val in hset:
    result.add val.makeInitCalls()

  result = newCall("toHashSet", result)

proc pprintCalls*(node: NimNode, level: int): void =
  let pref = "  ".repeat(level)
  let pprintKinds = {nnkCall, nnkPrefix, nnkBracket}
  case node.kind:
    of nnkCall:
      if ($node[0].toStrLit()).startsWith("make"):
        echo pref, "make", (($node[0].toStrLit())[4..^1]).toGreen()
      else:
        echo pref, $node[0].toStrLit()

      if node[1..^1].noneOfIt(it.kind in pprintKinds):
        echo pref, "  ",
          node[1..^1].mapIt($it.toStrLit()).join(", ").toYellow()
      else:
        for arg in node[1..^1]:
          pprintCalls(arg, level + 1)
    of nnkPrefix:
      echo pref, node[0]
      pprintCalls(node[1], level)
    of nnkBracket:
      for subn in node:
        pprintCalls(subn, level + 1)
    of nnkIdent:
      echo pref, ($node).toGreen()
    else:
      echo ($node.toStrLit()).indent(level * 2)
