import macros, tables, sets, typetraits, sequtils

type
  NType = object
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


func mkNType*(name: string, gparams: seq[string]): NType =
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

func toNTypeAst*[T](): NType =
  let str = $typeof(T)
  let expr = parseExpr(str)

func makeInitCalls*[T](val: T): NimNode =
  when T is enum:
    ident($val)
  else:
    newLit(val)

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
