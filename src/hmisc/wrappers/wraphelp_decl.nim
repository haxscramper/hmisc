import std/[macros, strutils, sequtils]

import ./wraphelp_store

import
  ../core/[all, code_errors],
  ../macros/argpass



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

func cxxTypeAux*(t: NimNode): CxxType =
  case t.kind:
    of nnkIdent:
      result = initCxxType(t.strVal())

    of nnkEmpty:
      result = initCxxType("void")

    of nnkPtrTy:
      result = cxxTypeAux(t[0]).wrap(ctkPtr)

    else:
      raise newImplementKindError(t)

func cxxArgAux(arg: NimNode): CxxArg =
  initCxxArg(arg[0].strVal(), arg[1].cxxTypeAux())

func procDeclAux(entry: NimNode, ctx: WrapCtx): CxxProc =
  let name =
    if entry.body.kind == nnkEmpty:
      entry.name().strVal()

    else:
      assertNodeKind(entry.body()[0], {nnkCall})
      entry.body()[0][0].strVal()

  result.nimName = name
  result.cxxName = ctx.namespace & name
  result.header = some initCxxHeader(ctx.header)

  var filter: seq[NimNode]
  result.returnType = entry.params()[0].cxxTypeAux()

  for arg in entry.params()[1 .. ^1]:
    result.arguments.add cxxArgAux(arg)

  for pr in entry.pragma:
    if pr.eqIdent("const"):
      result.isConst = true

    elif pr.eqIdent("constructor"):
      result.nimName = "new" & ctx.cxxClassName
      result.constructorOf = some ctx.cxxClassName

    else:
      filter.add pr

  entry.pragma = nnkPragma.newTree(filter)

  if ctx.inClass and not result.isConstructor:
    let thisTy =
      if result.isConst:
        ctx.nimClassName

      else:
        nnkVarTy.newTree(ctx.nimClassName)

  if result.isConstructor:
    if entry.params[0].kind == nnkPtrTy:
      result.icpp = "new " & ctx.getIcpp(ctx.cxxClassName, true) & "(@)"

    else:
      result.icpp = ctx.getIcpp(ctx.cxxClassName, true) & "(@)"

  else:
    if allIt(name, it in IdentChars):
      result.icpp = ctx.getIcpp(name & "(@)", false)

    else:
      result.icpp = ctx.getIcpp("operator" & name & "(@)", false)


  # result.addPragma newEcE(ident("importcpp"), newLit(icpp))


func stmtAux(entry: NimNode, ctx: WrapCtx): seq[CxxEntry]

func headerAux(name: string, body: seq[NimNode], ctx: WrapCtx): seq[CxxEntry] =
  var ctx = ctx
  ctx.header = name
  for node in body:
    result.add stmtAux(node, ctx)

func namespaceAux(name: string, body: seq[NimNode], ctx: WrapCtx): seq[CxxEntry] =
  var ctx = ctx
  ctx.namespace.add name
  for node in body:
    result.add stmtAux(node, ctx)

func splitClassName(name: NimNode):
  tuple[nimName: NimNode, cxxName: string, super: Option[NimNode]] =

  case name.kind:
    of nnkStrLit, nnkIdent:
      result.cxxName = name.strVal()
      result.nimName = ident(name.strVal())

    of nnkInfix:
      case name[0].strVal():
        of "as":
          result.cxxName = name[1].strVal()
          result.nimName = name[2]

        of "of":
          result.super = some name[2]
          let (nim, cxx, _) = splitClassName(name[1])
          result.nimName = nim
          result.cxxName = cxx

        else:
          raise newImplementError()

    else:
      raise newUnexpectedKindError(name)


proc flatStmtList(nodes: seq[NimNode]): seq[NimNode] =
  proc aux(node: NimNode): seq[NimNode] =
    if node of nnkStmtList:
      for sub in node:
        result.add aux(sub)

    else:
      result.add node

  for node in nodes:
    result.add aux(node)


func classAux(name: NimNode, body: seq[NimNode], ctx: WrapCtx): CxxObject =
  var ctx = ctx
  let (nim, cxx, super) = splitClassName(name)
  ctx.inClass = true
  ctx.nimClassName = nim
  if super.isSome():
    result.parent.add cxxTypeAux(super.get())

  ctx.cxxClassName = cxx

  result.nimName = nim.repr()
  result.cxxName = ctx.namespace & cxx
  result.icpp = ctx.getIcpp(ctx.cxxClassName, true)
  result.header = some initCxxHeader(ctx.header)

  for entry in body.flatStmtList():
    case entry.kind:
      of nnkProcDef:
        result.methods.add procDeclAux(entry, ctx)

      of nnkStmtList:
        for stmt in entry:
          result.nested.add stmtAux(stmt, ctx)

      else:
        raise newImplementKindError(entry)


  # result = newStmtList(
  #   nnkTypeSection.newTree(
  #     nnkTypeDef.newTree(
  #       nnkPragmaExpr.newTree(
  #         nnkPostfix.newTree(ident"*", nim),
  #         nnkPragma.newTree(
  #           newEcE("importcpp", newLit()),
  #           (if isByref: ident"byref" else: ident"bycopy"),
  #           ident("inheritable"),
  #           newEcE("header", newLit()))),
  #       newEmptyNode(),
  #       nnkObjectTy.newTree(
  #         newEmptyNode(),
  #         (if super.isSome(): nnkOfInherit.newTree(super.get()) else: newEmptyNode()),
  #         fieldList))) & resList)



func stmtAux(entry: NimNode, ctx: WrapCtx): seq[CxxEntry] =
  case entry.kind:
    of nnkProcDef:
      result.add procDeclAux(entry, ctx)

    of nnkCommand:
      let kind = entry[0].strVal()

      case kind:
        of "namespace":
          result = namespaceAux(entry[1].strVal(), entry[2..^1], ctx)

        of "class":
          result.add classAux(entry[1], entry[2..^1], ctx)

        of "static", "struct", "enum", "var", "let", "const":
          raise newImplementError(kind)


        else:
          raise newImplementKindError(kind)

    of nnkStmtList:
      for stmt in entry:
        result.add stmtAux(stmt, ctx)

    else:
      raise newUnexpectedKindError(entry, treeRepr(entry))

proc toNNode(t: CxxType): NimNode =
  case t.kind:
    of ctkIdent:
      result = ident(t.nimName)

    of ctkPtr:
      result = nnkPtrTy.newTree(t.wrapped.toNNode())

    else:
      raise newImplementKindError(t)

proc toNNode(arg: CxxArg): NimNode =
  nnkIdentDefs.newTree(ident(arg.nimName), arg.nimType.toNNode(), newEmptyNode())

proc toNNode(header: CxxHeader): NimNode =
  case header.kind:
    of chkGlobal: newLit(header.global)
    of chkAbsolute: newLit(header.file.string)
    of chkPNode: newLit(header.other)


proc toNNode(def: CxxProc, onConstructor: CxxTypeKind = ctkIdent): NimNode =
  let source =
    if def.header.isSome():
      newEcE("header", def.header.get().toNNode())

    else:
      newEmptyNode()

  nnkProcDef.newTree(
    nnkPostfix.newTree(ident("*"), ident(def.nimName)),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      def.getReturn(onConstructor).toNNode() & def.arguments.map(toNNode)),
    nnkPragma.newTree(
      newEcE("importcpp", def.icpp.newLit()), source),
    newEmptyNode(),
    newEmptyNode())


proc toNNode*(entry: CxxEntry): NimNode =
  result = newStmtList()
  case entry.kind:
    of cekObject:
      var fieldList = nnkRecList.newTree()
      let obj = entry.cxxObject
      result.add nnkTypeDef.newTree(
        nnkPragmaExpr.newTree(
          ident(obj.nimName),
          nnkPragma.newTree(
              ident("inheritable"),
              ident("byref"),
              newEcE("header", obj.header.get().toNNode()),
              newEcE("importcpp", obj.icpp.newLit()))),
        newEmptyNode(),
        nnkObjectTy.newTree(
          newEmptyNode(),
          tern(
            obj.parent.len == 0,
            newEmptyNode(),
            nnkOfInherit.newTree(obj.parent[0].toNNode())),
          fieldList))


      for meth in obj.methods:
        result.add meth.toNNode(ctkPtr)

      for n in obj.nested:
        result.add n.toNNode()

    of cekProc:
      result = entry.cxxProc.toNNode()

    else:
      raise newImplementKindError(entry)

proc toNNode*(entries: seq[CxxEntry]): NimNode =
  var types: seq[NimNode]
  var other: seq[NimNode]
  for item in entries:
    for conv in item.toNNode():
      if conv.kind == nnkTypeDef:
        types.add conv

      else:
        other.add conv

  result = newStmtList(nnkTypeSection.newTree(types))
  result.add other


macro wrapheader*(name: static[string], body: untyped): untyped =
  result = headerAux(name, toSeq(body), WrapCtx()).toNNode()
  echo result.repr()
