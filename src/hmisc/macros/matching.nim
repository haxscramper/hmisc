import sequtils, macros, tables, options, strformat, sugar, strutils,
       parseutils, hpprint

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

template hasKind*(head, kindExpr: untyped): untyped =
  hasKindImpl(head.kind, kindExpr)

type
  EStructKind = enum
    kItem
    kList
    kTuple
    kPairs
    kObject
    kSet

  ListKeyword = enum
    lkAny ## Any element from list
    lkAll ## All elements from list
    lkNone
    lkOpt
    lkUntil ## All elements until
    lkPos ## Exact position

  ListStructure = object
    decl: NimNode
    bindVar: Option[string]
    patt: Match
    kind: ListKeyword

  ItemMatchKind = enum
    imkInfixEq
    imkSubpatt
    imkPredicate

  KVPair = tuple[key: NimNode, patt: Match]
  Match = ref object
    bindVar: Option[string]
    declNode: NimNode
    case kind: EStructKind
      of kItem:
        case itemMatch: ItemMatchKind
          of imkInfixEq:
            infix: string
            rhsNode: NimNode
            isPlaceholder: bool
          of imkSubpatt:
            rhsPatt: Match
          of imkPredicate:
            isCall: bool
            predBody: NimNode

      of kList:
        listElems: seq[ListStructure]
      of kTuple:
        tupleElems: seq[Match]
      of kPairs:
        pairElems: seq[KVPair]

      of kSet:
        setElems: seq[Match]
      of kObject:
        kindCall: Option[string]
        fldElems: seq[tuple[
          name: string,
          patt: Match
        ]]

        kvMatches: Option[Match]
        subscriptMatches: Option[Match]


  EMatchKind = enum
    mkEq
    mkIn
    mkItExpr

  AccsElem = object
    variadicContext: bool
    case inStruct: EStructKind
      of kList, kTuple:
        idx: NimNode
      of kObject:
        fld: string
      of kPairs:
        parentKey: bool
        key: NimNode
      of kItem, kSet:
        discard

  Path = seq[AccsElem]

  VarKind = enum
    vkRegular
    vkSequence
    vkOption
    vkSet

  VarSpec = object
    decl {.requiresinit.}: NimNode
    varKind: VarKind
    hasAltContext: bool
    typePath: Path

  VarTable = Table[string, VarSpec]

func isNamedTuple(node: NimNode): bool =
  node.allOfIt(it.kind in {nnkExprColonExpr, nnkIdent}) and
  node.allOfIt((it.kind == nnkIdent) -> it.strVal == "_")

func isInfixPatt(node: NimNode): bool =
  node.kind == nnkInfix and node[0].strVal() in ["|"]

func newInfix(s: string, a, b: NimNode): NimNode =
  nnkInfix.newTree(ident s, a, b)

func newPrefix(s: string, a: NimNode): NimNode =
  nnkPrefix.newTree(ident s, a)

func isVariadic(path: Path): bool =
  path.anyOfIt(it.variadicContext)

func makeVarSet(v: string, expr: NimNode): NimNode =
  let
    varset = ident "varset"
    varname = ident v

  quote do:
    `varset`(`varname`, `expr`)

func foldInfix(s: seq[NimNode],
               inf: string, start: seq[NimNode] = @[]): NimNode =
  ( start & s ).foldl(newInfix(inf, newPar(a), b))

func toAccs(path: Path, name: string): NimNode =
  func aux(prefix: NimNode, top: Path): NimNode =
    let head = top[0]
    result = case head.inStruct:
      of kList, kTuple:
        nnkBracketExpr.newTree(prefix, top[0].idx)
      of kObject:
        nnkDotExpr.newTree(prefix, ident head.fld)
      of kPairs:
        nnkBracketExpr.newTree(prefix, head.key)
      of kItem:
        prefix
      of kSet:
        raiseAssert("#[ IMPLEMENT ]#")

    if top.len > 1:
      result = result.aux(top[1 ..^ 1])


  result =
    if path.len > 0:
      (ident name).aux(path)
    else:
      ident name


func parseMatchExpr(n: NimNode): Match

func parseKVTuple(n: NimNode): Match =
  result = Match(kind: kObject)
  for elem in n:
    discard

func parseListMatch(n: NimNode): seq[ListStructure] =
  for elem in n:
    case elem.kind:
      of nnkCall, nnkCommand:
        raiseAssert("#[ IMPLEMENT ]#")
      of nnkInfix, nnkPrefix, nnkIdent:
        var
          match = parseMatchExpr(elem)
          bindv = match.bindVar

        match.bindVar = none(string)

        result.add ListStructure(
          patt: match,
          bindVar: bindv,
          kind: lkPos)
      else:
        raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")

func parseTableMatch(n: NimNode): seq[KVPair] =
  discard

func parseMatchExpr(n: NimNode): Match =
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit:
      result = Match(kind: kItem, itemMatch: imkInfixEq)
      if n == ident "_":
        result.isPlaceholder = true
      else:
        result.rhsNode = n
        result.infix = "=="
    of nnkPar:
      if n.isNamedTuple():
        result = parseKVTuple(n)
      else:
        result = Match(kind: kTuple)
        for elem in n:
          result.tupleElems.add parseMatchExpr(elem)
    of nnkPrefix:
      if n[0].strVal() == "is":
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[1]))

      elif n[0].strVal() == "@":
        n[1].assertNodeKind({nnkIdent})
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, isPlaceholder: true,
          bindVar: some(n[1].strVal()),)

      else:
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, infix: n[0].strVal())

    of nnkBracket:
      result = Match(kind: kList, listElems: parseListMatch(n))
    of nnkTableConstr:
      result = Match(kind: kPairs, pairElems: parseTableMatch(n))
    of nnkCurly:
      discard
    of nnkObjConstr, nnkCall:
      result = parseKVTuple(n[1])
      result.kindCall = some(n[0].strVal())
    elif n.isInfixPatt():
      raiseAssert("#[ IMPLEMENT ]#")
    elif n.kind == nnkInfix:
      n[1].assertNodeKind({nnkIdent})
      if n[0].strVal() == "is":
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[1]))
      else:
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, infix: n[0].strVal())

      result.bindVar = some(n[1].strVal())
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")


func makeMatchExpr(m: Match, vt: var VarTable, path: Path): NimNode

func makeListMatch(list: Match, vt: var VarTable, path: Path): NimNode =
  var idx = 1
  while idx < list.listElems.len:
    if list.listElems[idx - 1].kind notin {lkUntil, lkPos, lkOpt}:
      raise ({
        list.listElems[idx - 1].decl : "Greedy list match pattern",
        list.listElems[idx].decl : "Must be last in sequence but found"
      }).toCodeError("Greedy list match must be last element in pattern")

  let
    posid = ident("pos")
    matched = genSym(nskVar, "matched")
    failBlock = ident("failBlock")
    failBreak = nnkBreakStmt.newTree(failBlock)


  result = newStmtList()
  for idx, elem in list.listElems:
    let expr = elem.patt.makeMatchExpr(vt, path & @[
      AccsElem(inStruct: kTuple, idx: posid)])

    case elem.kind:
      of lkPos:
        result.add quote do:
          if not `expr`:
            `failBreak`

      else:
        if true:
          raiseAssert("#[ IMPLEMENT ]#")

  result = quote do:
    var `matched` = false
    block `failBlock`:
      var `posid` = 0

      `result`

      `matched` = true

    `matched`



  result = result.newPar().newPar()




func makeMatchExpr(m: Match, vt: var VarTable, path: Path): NimNode =
  case m.kind:
    of kItem:
      let parent = path.toAccs("expr")

      case m.itemMatch:
        of imkInfixEq:
          if m.isPlaceholder:
            return newLit(true)
          else:
            let inf = newInfix(m.infix, m.rhsNode, parent)
            iflet (vname = m.bindVar):
              let bindVar = makeVarSet(vname, parent)
              return quote do:
                if `inf`:
                  `bindVar`
                  true
                else:
                  false
            else:
              return inf
        else:
          raiseAssert("#[ IMPLEMENT ]#")

    of kList:
      return makeListMatch(m, vt, path)
    of kTuple:
      let conds = collect(newSeq):
        for idx, it in m.tupleElems:
          it.makeMatchExpr(vt, path & @[
            AccsElem(inStruct: kTuple, idx: newLit(idx))
          ])

      return conds.foldInfix("and")
    else:
      raiseAssert("#[ IMPLEMENT ]#")




func makeMatchExpr(m: Match): tuple[expr: NimNode, vtable: VarTable] =
  debugpprint m
  result.expr = makeMatchExpr(m, result.vtable, @[])
  # debugecho result.expr.toStrLit().strVal()

func updateVarSet(nn: NimNode, vtable: VarTable): void =
  ## Recursively walk generate pattern match and replace dummy
  ## variable assignments with correct code
  for idx, node in nn:
    if node.kind == nnkCall and
       node[0] == ident "varset":
      let
        varn = node[1]
        expr = node[2]

      case vtable[varn.strVal()].varKind:
        of vkSequence:
          nn[idx] = quote do:
            `varn`.add `expr`

        of vkOption:
          nn[idx] = quote do:
            `varn` = some(`expr`)

        of vkSet:
          nn[idx] = quote do:
            `varn`.incl some(`expr`)

        of vkRegular:
          nn[idx] = quote do:
            `varn` = `expr`

    else:
      updateVarSet(nn[idx], vtable)

func toNode(input: tuple[expr: NimNode, vtable: VarTable]): NimNode =
  var (expr, vtable) = input

  var exprNew = nnkStmtList.newTree()
  for name, spec in vtable:
    let typeExpr = toAccs(spec.typePath, "expr")
    case spec.varKind:
      of vkSequence:
        exprNew.add quote do:
          var `name`: seq[typeof(`typeExpr`)]

      of vkOption:
        exprNew.add quote do:
          var `name`: Option[typeof(`typeExpr`)]

      of vkSet, vkRegular:
        exprNew.add quote do:
          var `name`: typeof(`typeExpr`)

  updateVarSet(expr, vtable)
  return quote do:
    `exprNew`
    `expr`


macro match*(
  n: tuple | object | ref object | seq | array | set): untyped =
  var matchcase = nnkIfStmt.newTree()
  for elem in n[1 .. ^1]:
    case elem.kind:
      of nnkOfBranch:
        if elem[0] == ident "_":
          elem[0].raiseCodeError(
            "To create catch-all match use `else` clause",
            "Replace `_` with `else` here")


        matchcase.add nnkElifBranch.newTree(
          elem[0].parseMatchExpr().makeMatchExpr().toNode(),
          elem[1]
        )

      of nnkElifBranch, nnkElse:
        matchcase.add elem
      else:
        discard
        # raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")

  let head = n[0]
  result = quote do:
    block:
      let expr {.inject.} = `head`
      let pos {.inject.}: int = 0
      `matchcase`

  debugecho result
