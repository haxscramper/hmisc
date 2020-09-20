import sequtils, macros, tables, options, strformat, sugar, strutils,
       parseutils

import ../helpers
import iflet
import ../hexceptions

template `->`(a, b: bool): bool = (if a: b else: true)

func parseEnumField*(fld: NimNode): string =
  case fld.kind:
    of nnkEnumFieldDef:
      fld[0].strVal
    of nnkSym:
      fld.strVal
    else:
      raiseAssert(&"#[ IMPLEMENT {fld.kind} ]#")

func parseEnumImpl*(en: NimNode): seq[string] =
  case en.kind:
    of nnkSym:
      let impl = en.getTypeImpl()
      case impl.kind:
        of nnkBracketExpr:
          return parseEnumImpl(impl.getTypeInst()[1].getImpl())
        of nnkEnumTy:
          result = parseEnumImpl(impl)
        else:
          raiseAssert(&"#[ IMPLEMENT {impl.kind} ]#")
    of nnkTypeDef:
      result = parseEnumImpl(en[2])
    of nnkEnumTy:
      for fld in en[1..^1]:
        result.add parseEnumField(fld)
    of nnkTypeSection:
      result = parseEnumImpl(en[0])
    else:
      raiseAssert(&"#[ IMPLEMENT {en.kind} ]#")


func pref*(name: string): string =
  discard name.parseUntil(result, {'A' .. 'Z', '0' .. '9'})

macro hasKindImpl*(head: typed, kind: untyped): untyped =
  let
    impl = head.getTypeImpl().parseEnumImpl()
    pref = impl.commonPrefix().pref()
    names = impl.dropPrefix(pref)
    kind = ident(kind.toStrLit().strVal().addPrefix(pref))

  result = nnkInfix.newTree(ident "==", head, kind)
  echov result

template hasKind*(head, kindExpr: untyped): untyped =
  hasKindImpl(head.kind, kindExpr)

type
  EStructKind = enum
    kItem
    kList
    kTuple
    kPairs
    kObject

  EMatchKind = enum
    mkEq
    mkIn
    mkItExpr

  AccsElem = object
    case inStruct: EStructKind
      of kList, kTuple:
        idx: int
      of kObject:
        fld: string
      of kPairs:
        parentKey: bool
        key: NimNode
      of kItem:
        discard

  Path = seq[AccsElem]

  VarSpec = object
    name: string
    isList: bool
    firstDecl: NimNode

  VarTable = Table[string, VarSpec]

  Case = object
    expr: NimNode
    heads: seq[NimNode]
    default: Option[NimNode]

func isNamedTuple(node: NimNode): bool =
  node.allOfIt(it.kind in {nnkExprColonExpr, nnkIdent}) and
  node.allOfIt((it.kind == nnkIdent) -> it.strVal == "_")

# func isKindCall(node: NimNode): bool =
#   node[0].kind == nnkIdent and node[0].strVal()[0].isUpperAscii()

func isInfixPatt(node: NimNode): bool =
  node.kind == nnkInfix and node[0].strVal() in ["|"]

func toAccs(path: Path, name: string): NimNode =
  func aux(prefix: NimNode, top: Path): NimNode =
    let head = top[0]
    result = case head.inStruct:
      of kList, kTuple:
        nnkBracketExpr.newTree(prefix, newLit(top[0].idx))
      of kObject:
        nnkDotExpr.newTree(prefix, ident head.fld)
      of kPairs:
        nnkBracketExpr.newTree(prefix, head.key)
      of kItem:
        prefix

    if top.len > 1:
      result = result.aux(top[1 ..^ 1])


  result =
    if path.len > 0:
      (ident name).aux(path)
    else:
      ident name

  # echov result

type ExprRes = tuple[
  node: NimNode, vars: seq[tuple[name: string, path: Path]]]

func makeMatchExpr(n: NimNode, path: Path): ExprRes =
  # echov path, "Make match expression"
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit:
      if n == ident "_":
        result.node = ident("true")
      else:
        result.node = nnkInfix.newTree(ident "==", path.toAccs("expr"), n)
    of nnkPar:
      let conds: seq[ExprRes] =
        if n.isNamedTuple():
          collect(newSeq):
            for idx, kv in n:
              if kv.kind == nnkIdent:
                makeMatchExpr(kv, path & @[
                  AccsElem(inStruct: kTuple, idx: idx)
                ])
              else:
                echov kv[1], kv[0].strVal()
                let val = makeMatchExpr(kv[1], path & @[
                  AccsElem(inStruct: kObject, fld: kv[0].strVal())
                ])

                val
        else:
          collect(newSeq):
            for idx, val in n:
              makeMatchExpr(val, path & @[
                AccsElem(inStruct: kTuple, idx: idx)
              ])

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

      # echov result.node

    of nnkPrefix:
      let conds: seq[ExprRes] =
        if n[0].strVal() == "$":
          let
            accs = path.toAccs("expr")
            vars = n[1]
            node = quote do:
              ((((
                block:
                  `vars` = `accs`
                  true
              ))))

          @[(node, @[(n[1].strVal(), path)])]
        else:
          n.raiseCodeError("Unexpected prefix")

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

    of nnkBracket:
      let pattSize = newLit(n.len)
      let conds =
        collect(newSeq):
          for idx, elem in n:
            let
              parent = path.toAccs("expr")
              key = newLit(idx)
              (subexp, vars) = elem.makeMatchExpr(path & @[
                AccsElem(inStruct: kList, idx: idx)
              ])

            let node = quote do:
              ((((
                block:
                  if `parent`.len == `pattSize`:
                    # let it {.inject.} = expr[`key`]
                    # let expr {.inject.} = `parent`[`key`]
                    `subexp`
                  else:
                    false
              ))))

            (node, vars)

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

    of nnkTableConstr:
      let conds: seq[ExprRes] = collect(newSeq):
        for kv in n:
          let
            parent = path.toAccs("expr")
            key = kv[0]
            (subexp, vars) = kv[1].makeMatchExpr(path & @[
                AccsElem(inStruct: kPairs, key: kv[0])
            ])

          let node = quote do:
            ((
              block:
                if `key` in `parent`:
                  # let expr {.inject.} = `parent`[`key`]
                  `subexp`
                else:
                  false
            ))

          (node, vars)

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()


    elif n.kind in {nnkObjConstr, nnkCall}:
      let kindCheck = newCall(ident "hasKind", path.toAccs("expr"), n[0])

      var conds: seq[ExprRes]
      conds.add (newCall(ident "hasKind", path.toAccs("expr"), n[0]), @[])

      # echov struct
      for idx, kv in n[1..^1]:
        # echov path
        let parent = path.toAccs("expr")
        echov kv


        let tmp = kv[1].makeMatchExpr(path &
          ((
            block:
              if kv.kind != nnkBracket:
                @[ AccsElem(inStruct: kObject, fld: kv[0].strVal()) ]
              else:
                @[]
          ))
        )
        conds.add tmp


      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

      # echov result.node

    elif n.isInfixPatt():
      let conds: seq[ExprRes] = collect(newSeq):
        for patt in n[1..^1]:
          patt.makeMatchExpr(path)

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "or", a, b)).concatSide()
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")



func makeMatch(n: NimNode, path: Path): NimNode =
  result = nnkIfStmt.newTree()
  for elem in n[1 ..^ 1]:
    case elem.kind:
      of nnkOfBranch:
        let (expr, vars) = makeMatchExpr(elem[0], path)

        let varDecsl = vars.deduplicateIt(it.name)
        var exprNew = nnkStmtList.newTree()
        for v in vars:
          let
            name = ident(v.name)
            typeExpr = toAccs(v.path, "expr")

          exprNew.add quote do:
            var `name`: typeof(`typeExpr`)

        exprNew.add expr

        result.add nnkElifBranch.newTree(exprNew, elem[1])
      of nnkElifBranch, nnkElse:
        result.add elem
      else:
        block:
          raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")


macro match*(
  n: tuple | object | ref object | seq | array): untyped =
  var cs: Case
  cs.expr = n[0]
  for elem in n[1 .. ^1]:
    case elem.kind:
      of nnkOfBranch, nnkElifBranch:
        cs.heads.add elem[0]
      of nnkElse:
        cs.default = some(elem)
      else:
        raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")

  for head in n[1 ..^  1]:
    if head.kind == nnkOfBranch:
      let head = head[0]
      if head == ident "_":
        raiseCodeError(head,
                       "To create catch-all match use `else` clause",
                       "Replace `_` with `else` here"
        )

  # Generate matcher expressions
  let matchcase = n.makeMatch(@[])

  # dieHere()


  let head = cs.expr
  result = quote do:
    block:
      let expr {.inject.} = `head`
      # let input {.inject.} = `inputExpr`
      # ifHaxComp:
      #   echo typeof(input)

      `matchcase`

  haxThis result.toStrLit()
  # dieHereMacro()
  # assert inputExpr != nil
