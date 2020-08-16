import macros, tables, sets, typetraits, sequtils, strformat, strutils,
       options

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


func `$`*(nt: NType): string =
  if nt.genParams.len > 0:
    nt.head & "[" & nt.genParams.mapIt($it).join(", ") & "]"
  else:
    nt.head


func mkNType*(name: string, gparams: seq[string] = @[]): NType =
  NType(head: name, genParams: gparams.mapIt(mkNType(it, @[])))

func mkNType*(name: string, gparams: openarray[NType]): NType =
  NType(head: name, genParams: toSeq(gparams))

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
