import
  std/[macros, sequtils, strutils]

import ../macros/argpass

import
  ".."/[base_errors, hexceptions]

func closureToCdecl*[T0, T1](
    cb: proc(a: var T0, b: T1) {.closure.}
  ): proc(a: var T0, b: T1, env: pointer) {.cdecl.} =

  discard

type
  StdInitializerList*[T] {.
    importcpp: "std::initializer_list",
    header: "<initializer_list>"
  .} = object

  CxxTemplateUndefined* = object

  cchar16* = uint16
  cchar32* = uint32
  cwchar* = uint32
  nullptr_t* = typeof(nil)

  StdNullptrT* = nullptr_t
  StdSizeT* = culong
  StdPtrdiffT* = clong

proc `as`*[T](
  undef: CxxTemplateUndefined, asTarget: typedesc[T]): T {.importcpp: "(#)"}

proc cxxInitList*[T](args: T) {.importcpp: "{@}", varargs.}


proc newImportAux*() {.importc: "//", header: "<new>".} =
  discard

macro `//`*(arg: string): untyped =
  ## Emit C comment in generated source code
  ##
  ## `// "C comment"` will yield `/* C comment */` emited.
  let lit = newLit("/* " & arg.strVal() & " */")

  quote do:
    {.emit: `lit`.}

type
  UArray*[T] = UncheckedArray[T]
  PUarray*[T] = ptr UncheckedArray[T]

template `+`*[T](p: ptr T, offset: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[ByteAddress](p) +% int(offset) * sizeof(p[]))

template `+=`*[T](p: ptr T, offset: SomeInteger) =
  p = p + offset

template `-`*[T](p: ptr T, offset: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[ByteAddress](p) -% int(offset) * sizeof(p[]))

template `-=`*[T](p: ptr T, offset: SomeInteger) =
  p = p - offset

template `[]`*[T](p: ptr T, offset: SomeInteger): T =
  (p + offset)[]

template `[]=`*[T](p: ptr T, offset: SomeInteger, val: T) =
  (p + offset)[] = val

template toPUarray*[T](p: ptr T): PUarray[T] = cast[PUarray[T]](p)
template toPtr*[T](p: PUArray[T]): ptr T = cast[ptr T](p)

template toPtr*[T](r: ref T): ptr T = cast[ptr T](r)
template toPUarray*[T](r: ref T): PUarray[T] = cast[PUarray[T]](r)

template subArrayPtr*[T](arr: PUArray[T], idx: SomeInteger): PUarray[T] =
  toPUarray(toPtr(arr) + idx)


type
  WrapCtx = object
    namespace: seq[string]
    inClass: bool
    nimClassName: NimNode
    cxxClassName: string
    header: string

func getIcpp(ctx: WrapCtx, name: string, isType: bool): string =
  if ctx.inClass:
    if isType:
      join(ctx.namespace & name, "::")

    else:
      "#." & name

  else:
    join(ctx.namespace & name, "::")

func procDeclAux(entry: NimNode, ctx: WrapCtx): NimNode =
  let name =
    if entry.body.kind == nnkEmpty:
      entry.name().strVal()

    else:
      assertNodeKind(entry.body()[0], {nnkCall})
      entry.body()[0][0].strVal()

  result = entry
  result.body = newEmptyNode()
  if ctx.inClass:
    result.params.insert(1, nnkIdentDefs.newTree(
      ident("this"), ctx.nimClassName, newEmptyNode()))

  result.addPragma nnkExprColonExpr.newTree(
    ident("importcpp"), ctx.getIcpp(name & "(@)", false).newLit())


func stmtAux(entry: NimNode, ctx: WrapCtx): NimNode

func headerAux(name: string, body: seq[NimNode], ctx: WrapCtx): NimNode =
  var ctx = ctx
  ctx.header = name
  result = newStmtList()
  for node in body:
    result.add stmtAux(node, ctx)

func namespaceAux(name: string, body: seq[NimNode], ctx: WrapCtx): NimNode =
  var ctx = ctx
  ctx.namespace.add name
  result = newStmtList()
  for node in body:
    result.add stmtAux(node, ctx)

func splitClassName(name: NimNode): tuple[nimName: NimNode, cxxName: string] =
  case name.kind:
    of nnkStrLit:
      result.cxxName = name.strVal()
      result.nimName = ident(name.strVal())

    of nnkInfix:
      assert name[0].strVal() == "as"
      result.cxxName = name[1].strVal()
      result.nimName = name[2]

    else:
      raise newUnexpectedKindError(name)



func classAux(name: NimNode, body: seq[NimNode], ctx: WrapCtx): NimNode =
  var resList: seq[NimNode]
  var fieldList = nnkRecList.newTree()
  var ctx = ctx
  let (nim, cxx) = splitClassName(name)
  ctx.inClass = true
  ctx.nimClassName = nim
  ctx.cxxClassName = cxx

  for entry in body:
    case entry.kind:
      of nnkProcDef:
        resList.add procDeclAux(entry, ctx)

      of nnkStmtList:
        for stmt in entry:
          resList.add stmtAux(stmt, ctx)

      else:
        raise newImplementKindError(entry)

  result = newStmtList(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        nnkPragmaExpr.newTree(
          nnkPostfix.newTree(ident"*", nim),
          nnkPragma.newTree(
            newEcE("importcpp", newLit(ctx.getIcpp(ctx.cxxClassName, true))),
            newEcE("header", newLit(ctx.header)))),
        newEmptyNode(),
        nnkObjectTy.newTree(
          newEmptyNode(),
          newEmptyNode(),
          fieldList))) & resList)



func stmtAux(entry: NimNode, ctx: WrapCtx): NimNode =
  case entry.kind:
    of nnkProcDef:
      result = procDeclAux(entry, ctx)

    of nnkCommand:
      let kind = entry[0].strVal()

      case kind:
        of "namespace":
          result = namespaceAux(entry[1].strVal(), entry[2..^1], ctx)

        of "class":
          result = classAux(entry[1], entry[2..^1], ctx)

        else:
          raise newImplementKindError(kind)

    of nnkStmtList:
      result = newStmtList()
      for stmt in entry:
        result.add stmtAux(stmt, ctx)

    else:
      raise newUnexpectedKindError(entry, treeRepr(entry))

macro wrapheader*(name: static[string], body: untyped): untyped =
  result = headerAux(name, toSeq(body), WrapCtx())
  echo result.repr()
