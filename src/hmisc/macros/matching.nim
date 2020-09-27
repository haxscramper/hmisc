import sequtils, macros, tables, options, strformat, sugar, strutils,
       parseutils
        # hpprint

# import ../types/colorstring
import ../helpers
import iflet
import ../hexceptions

template `->`(a, b: bool): bool = (if a: b else: true)

var debugTraceMatch* {.compiletime.}: bool = false

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
    kAlt

  ListKeyword = enum
    lkAny = "any" ## Any element from list
    lkAll = "all" ## All elements from list
    lkNone = "none"
    lkOpt = "opt"
    lkUntil = "until" ## All elements until
    lkPos ## Exact position

  ListStructure = object
    decl: NimNode
    bindVar: Option[NimNode]
    patt: Match
    kind: ListKeyword

  ItemMatchKind = enum
    imkInfixEq
    imkSubpatt
    imkPredicate

  KVPair = tuple[key: NimNode, patt: Match]
  Match = ref object
    bindVar: Option[NimNode]
    declNode {.requiresinit.}: NimNode
    case kind: EStructKind
      of kItem:
        case itemMatch: ItemMatchKind
          of imkInfixEq:
            infix: string
            rhsNode: NimNode
            isPlaceholder: bool
            isOptional: bool
          of imkSubpatt:
            rhsPatt: Match
          of imkPredicate:
            isCall: bool
            predBody: NimNode

      of kAlt:
        altElems: seq[Match]
      of kList:
        listElems: seq[ListStructure]
      of kTuple:
        tupleElems: seq[Match]
      of kPairs:
        pairElems: seq[KVPair]

      of kSet:
        setElems: seq[Match]
      of kObject:
        kindCall: Option[NimNode]
        fldElems: seq[tuple[
          name: string,
          patt: Match
        ]]

        kvMatches: Option[Match]
        listMatches: Option[Match]

  MatchTrace* = object
    pattern*: string
    input*: string
    success*: bool
    subtrace*: seq[MatchTrace]


  AccsElem = object
    isVariadic: bool
    case inStruct: EStructKind
      of kList:
        pos: NimNode
      of kTuple:
        idx: int
      of kObject:
        fld: string
      of kPairs:
        parentKey: bool
        key: NimNode
      of kSet, kAlt:
        discard
      of kItem:
        isOpt: bool

  Path = seq[AccsElem]

  VarKind = enum
    vkRegular
    vkSequence
    vkOption
    vkSet
    vkAlt

  VarSpec = object
    decl {.requiresinit.}: NimNode
    varKind: VarKind
    typePath: Path

  VarTable = Table[string, VarSpec]

func register*(
  trace: var MatchTrace, expr, item: string, ok: bool) =
  trace.subtrace.add MatchTrace(
    success: ok, pattern: expr, input: item)


func register*(
  trace: var MatchTrace, subtrace: MatchTrace) =
  trace.subtrace.add subtrace

func `$`*(tr: MatchTrace, level: int = 0): string =
  result = "  ".repeat(level) & tr.pattern & " " &
    tr.input & " " & tr.success.tern("\e[32mok\e[39m", "\e[31mfail\e[39m")

  if tr.subtrace.len > 0:
    result &= "\n" & tr.subtrace.mapIt(`$`(it, level + 1)).join("\n")

func isNamedTuple(node: NimNode): bool =
  node.allOfIt(it.kind in {nnkExprColonExpr, nnkIdent}) and
  node.allOfIt((it.kind == nnkIdent) -> it.strVal == "_")

func isInfixPatt(node: NimNode): bool =
  node.kind == nnkInfix and node[0].strVal() in ["|"]

func newInfix(s: string, a, b: NimNode): NimNode =
  nnkInfix.newTree(ident s, a, b)

func newPrefix(s: string, a: NimNode): NimNode =
  nnkPrefix.newTree(ident s, a)


func makeVarSet(v: NimNode, expr: NimNode): NimNode =
  v.assertNodeKind({nnkIdent})
  newCall(ident "varset", v, expr)

func foldInfix(s: seq[NimNode],
               inf: string, start: seq[NimNode] = @[]): NimNode =
  ( start & s ).mapIt(it.newPar().newPar()).foldl(newInfix(inf, a, b))

func toAccs(path: Path, name: string): NimNode =
  func aux(prefix: NimNode, top: Path): NimNode =
    let head = top[0]
    result = case head.inStruct:
      of kList:
        nnkBracketExpr.newTree(prefix, top[0].pos)
      of kTuple:
        nnkBracketExpr.newTree(prefix, newLit(top[0].idx))
      of kObject:
        nnkDotExpr.newTree(prefix, ident head.fld)
      of kPairs:
        nnkBracketExpr.newTree(prefix, head.key)
      of kItem, kAlt:
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
  result = Match(kind: kObject, declNode: n)
  var start = 0
  if n.kind in {nnkCall, nnkObjConstr}:
    start = 1
    result.kindCall = some(n[0])

  for elem in n[start .. ^1]:
    case elem.kind:
      of nnkExprColonExpr:
        elem[0].assertNodeKind({nnkIdent})
        result.fldElems.add((
          elem[0].strVal(),
          elem[1].parseMatchExpr()))
      of nnkBracket:
        result.listMatches = some(elem.parseMatchExpr())
      else:
        elem.assertNodeKind({nnkExprColonExpr})

func contains(kwds: openarray[ListKeyword], str: string): bool =
  for kwd in kwds:
    if eqIdent($kwd, str):
      return true

func parseListMatch(n: NimNode): seq[ListStructure] =
  for elem in n:
    let (elem, opKind) =
      if elem.kind in {nnkCall, nnkCommand} and elem[0].strVal() in [
        lkAny, lkAll, lkNone, lkOpt, lkUntil]:
        var kwd: ListKeyword
        for (key, val) in {
          "any" : lkAny,
          "all" : lkAll,
          "opt" : lkOpt,
          "until" : lkUntil,
          "none" : lkNone
            }:
          if elem[0].eqIdent(key):
            kwd = val
            break


        (elem[1], kwd)
      else:
        (elem, lkPos)

    var
      match = parseMatchExpr(elem)
      bindv = match.bindVar

    match.bindVar = none(NimNode)

    result.add ListStructure(patt: match, bindVar: bindv, kind: opKind)

func parseTableMatch(n: NimNode): seq[KVPair] =
  for elem in n:
    result.add((elem[0], elem[1].parseMatchExpr()))

func parseMatchExpr(n: NimNode): Match =
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit:
      result = Match(kind: kItem, itemMatch: imkInfixEq, declNode: n)
      if n == ident "_":
        result.isPlaceholder = true
      else:
        result.rhsNode = n
        result.infix = "=="
    of nnkPar:
      if n.isNamedTuple():
        result = parseKVTuple(n)
      else:
        result = Match(kind: kTuple, declNode: n)
        for elem in n:
          result.tupleElems.add parseMatchExpr(elem)
    of nnkPrefix:
      if n[0].strVal() == "is":
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[1]), declNode: n)

      elif n[0].strVal() == "@":
        n[1].assertNodeKind({nnkIdent})
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, isPlaceholder: true,
          bindVar: some(n[1]), declNode: n)

      else:
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, infix: n[0].strVal(),
          rhsNode: n[1], declNode: n
        )

    of nnkBracket:
      result = Match(
        kind: kList, listElems: parseListMatch(n), declNode: n)
    of nnkTableConstr:
      result = Match(
        kind: kPairs, pairElems: parseTableMatch(n), declNode: n)
    of nnkCurly:
      result = Match(kind: kSet, declNode: n)
      for node in n:
        node.assertNodeKindNot({nnkExprColonExpr})
        result.setElems.add parseMatchExpr(node)
    of nnkObjConstr, nnkCall:
      result = parseKVTuple(n)
    elif n.isInfixPatt():
      let
        lhs = n[1].parseMatchExpr()
        rhs = n[2].parseMatchExpr()

      var alts: seq[Match]
      if lhs.kind == kAlt: alts.add lhs.altElems else: alts.add lhs
      if rhs.kind == kAlt: alts.add rhs.altElems else: alts.add rhs
      result = Match(kind: kAlt, altElems: alts, declNode: n)
    elif n.kind == nnkInfix:
      n[1].assertNodeKind({nnkPrefix})
      n[1][1].assertNodeKind({nnkIdent})
      if n[0].strVal() == "is":
        echov n
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[2]), declNode: n)

      else:
        result = Match(
          kind: kItem, itemMatch: imkInfixEq,
          infix: n[0].strVal(), declNode: n)

      result.bindVar = some(n[1][1])
      # echov pstring result
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")

func isVariadic(p: Path): bool = p.anyOfIt(it.isVariadic)

func isAlt(p: Path): bool = p.anyOfIt(it.inStruct == kAlt)

func isOption(p: Path): bool = p.anyOfIt(
  it.inStruct == kItem and it.isOpt)

func classifyPath(path: Path): VarKind =
  # echov pstring path
  if path.isVariadic:
    vkSequence
  elif path.isAlt:
    vkAlt
  else:
    vkREgular

func addvar(tbl: var VarTable, vsym: NimNode, path: Path): void =
  if vsym.strVal() notin tbl:
    tbl[vsym.strVal()] = VarSpec(
      decl: vsym,
      varKind: path.classifyPath(),
      typePath: path
    )


func makeMatchExpr(
  m: Match, vt: var VarTable, path: Path, trace: Option[NimNode]): NimNode

func makeListMatch(
  list: Match, vt: var VarTable, path: Path, trace: Option[NimNode]): NimNode =
  var idx = 1
  while idx < list.listElems.len:
    if list.listElems[idx - 1].kind notin {lkUntil, lkPos, lkOpt}:
      raise ({
        list.listElems[idx - 1].decl : "Greedy list match pattern",
        list.listElems[idx].decl : "Must be last in sequence but found"
      }).toCodeError("Greedy list match must be last element in pattern")

    inc idx

  let
    posid = genSym(nskVar, "pos")
    matched = genSym(nskVar, "matched")
    failBlock = ident("failBlock")
    failBreak = nnkBreakStmt.newTree(failBlock)
    getLen = newCall("len", path.toAccs("expr"))


  var subtrace: Option[NimNode]
  iflet (tr = trace):
    subtrace = some genSym(nskVar, "subtrace")

  result = newStmtList()
  var minLen = 0
  var maxLen = 0
  for idx, elem in list.listElems:
    let
      parent = path & @[AccsElem(
        inStruct: kList, pos: posid,
        isVariadic: elem.kind notin {lkPos})]

      expr = elem.patt.makeMatchExpr(vt, parent, subtrace)


    result.add newCommentStmtNode(
      $elem.kind & " " & elem.patt.declNode.repr)


    var
      traceOk = newEmptyNode()
      traceErr = newEmptyNode()

    iflet (tr = subtrace):
      let parent = parent.toAccs("expr")
      let expr = newLit(elem.patt.declNode.repr)
      traceOk = quote do:
        `tr`.register(`expr`, $`parent`, true)

      traceErr = quote do:
        `tr`.register(`expr`, $`parent`, false)


    case elem.kind:
      of lkPos:
        inc minLen
        inc maxLen
        iflet (bindv = elem.bindVar):
          result.add makeVarSet(bindv, parent.toAccs("expr"))
          vt.addvar(bindv, parent)

        if elem.patt.kind == kItem and
           elem.patt.itemMatch == imkInfixEq and
           elem.patt.isPlaceholder:
          result.add newCall(ident "inc", posid)
        else:
          result.add quote do:
            if `expr`:
              `traceOk`
              inc `posid`
            else:
              `traceErr`
              `failBreak`

      else:
        maxLen = 5000
        var varset = newEmptyNode()

        iflet (bindv = elem.bindVar):
          varset = makeVarSet(bindv, parent.toAccs("expr"))
          vt.addvar(bindv, parent)

        case elem.kind:
          of lkAll:
            result.add quote do:
              block:
                var allOk: bool = true
                while `posid` < `getLen` and allOk:
                  if not `expr`:
                    allOk = false
                    `traceErr`
                  else:
                    `varset`
                    `traceOk`
                    inc `posid`

                if not allOk:
                  break `failBlock`

          of lkUntil:
            result.add quote do:
              while (`posid` < `getLen`) and (not `expr`):
                `varset`
                `traceOk`
                inc `posid`
          else:
            if false:
              raiseAssert("#[ IMPLEMENT ]#")


  let
    comment = newCommentStmtNode(list.declNode.repr)
    minNode = newLit(minLen)
    maxNode = newLit(maxLen)
    setCheck =
      if maxLen >= 5000:
        quote do:
          `getLen` < `minNode`
      else:
        quote do:
          `getLen` notin {`minNode` .. `maxNode`}

  var storeTrace = newEmptyNode()
  iflet (tr = trace):
    let
      subtrace = subtrace.get()
      parent = path.toAccs("expr")
      expr = newLit(list.declNode.repr)

    storeTrace = quote do:
      `tr`.register(`expr`, $`parent`, `matched`)
      `tr`.register `subtrace`

  var declTrace = newEmptyNode()
  iflet (tr = trace):
    let subtrace = subtrace.get()
    declTrace = quote do:
      var `subtrace`: MatchTrace

  result = quote do:
    `comment`
    var `matched` = false
    `declTrace`
    block `failBlock`:
      var `posid` = 0 ## Start list match

      if `setCheck`:
        ## fail on seq len
        break `failBlock`

      `result`

      `matched` = true ## List match ok

    `storeTrace`
    `matched`



  result = result.newPar().newPar()
  # debugecho result.repr




func makeMatchExpr(
  m: Match, vt: var VarTable, path: Path,
  trace: Option[NimNode]): NimNode =
  case m.kind:
    of kItem:
      let parent = path.toAccs("expr")
      # echov pstring m
      case m.itemMatch:
        of imkInfixEq, imkSubpatt:
          let inf =
            if m.itemMatch == imkInfixEq:
              if m.isPlaceholder:
                newLit(true)
              else:
                newInfix(m.infix, parent, m.rhsNode)
             else:
               makeMatchExpr(m.rhsPatt, vt, path, trace)

          iflet (vname = m.bindVar):
            # echov m.bindVar
            vt.addvar(vname, path)
            let bindVar = makeVarSet(vname, parent)
            if inf == newLit(true):
              return quote do:
                (`bindVar`; true)
            else:
              return quote do:
                block:
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
      return makeListMatch(m, vt, path, trace)
    of kTuple:
      let conds = collect(newSeq):
        for idx, it in m.tupleElems:
          it.makeMatchExpr(vt, path & @[
            AccsElem(inStruct: kTuple, idx: idx)
          ], trace)

      return conds.foldInfix("and")
    of kObject:
      var conds: seq[NimNode]
      for (fld, patt) in m.fldElems:
        conds.add patt.makeMatchExpr(vt, path & @[
          AccsElem(inStruct: kObject, fld: fld)], trace)

      iflet (list = m.listMatches):
        conds.add list.makeMatchExpr(vt, path, trace)

      iflet (kv = m.kvMatches):
        conds.add kv.makeMatchExpr(vt, path, trace)

      iflet (kc = m.kindCall):
        conds.add newCall(ident "hasKind", path.toAccs("expr"), kc)

      return conds.foldInfix("and")

    of kPairs:
      var conds: seq[NimNode]
      for (key, val) in m.pairElems:
        conds.add newInfix(
          "and",
          newInfix("in", key, path.toAccs("expr")),
          val.makeMatchExpr(vt, path & @[
            AccsElem(inStruct: kPairs, key: key)], trace))

      return conds.foldInfix("and")
    of kAlt:
      let conds = collect(newSeq):
        for alt in m.altElems:
          alt.makeMatchExpr(vt, path & @[AccsElem(inStruct: kAlt)], trace)

      return conds.foldInfix("or")
    else:
      raiseAssert("#[ IMPLEMENT ]#")




func makeMatchExpr(m: Match, trace: Option[NimNode]): tuple[
    expr: NimNode, vtable: VarTable] =

  result.expr = makeMatchExpr(m, result.vtable, @[], trace)

func updateTypeof(nn: NimNode): void =
  for idx, node in nn:
    if node.kind == nnkSym and node.strVal[^"pos"]:
      nn[idx] = newLit(0)
    else:
      nn[idx].updateTypeof()

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
            `varn`.add `expr` ## Append item to sequence

        of vkOption:
          nn[idx] = quote do:
            `varn` = some(`expr`) ## Set optional value

        of vkSet:
          nn[idx] = quote do:
            `varn`.incl some(`expr`) ## Add element to set

        of vkRegular:
          nn[idx] = nnkAsgn.newTree(varn, expr)

        of vkAlt:
          raiseAssert("#[ IMPLEMENT ]#")

    elif node.kind == nnkIfStmt and
         node[0][0].kind in {nnkSym, nnkIdent} and
         node[0][0].strVal == "true":
        nn[idx] = nn[idx][0][1]
        updateVarSet(nn[idx], vtable)
    else:
      updateVarSet(nn[idx], vtable)

func toNode(input: tuple[expr: NimNode, vtable: VarTable]): NimNode =
  var (expr, vtable) = input

  var exprNew = nnkStmtList.newTree()
  for name, spec in vtable:
    let vname = ident(name)
    # debugecho vname.lispRepr()
    let typeExpr = toAccs(spec.typePath, "expr")
    updateTypeof(typeExpr)
    case spec.varKind:
      of vkSequence:
        exprNew.add quote do:
          var `vname`: seq[typeof(`typeExpr`)]

      of vkOption:
        exprNew.add quote do:
          var `vname`: Option[typeof(`typeExpr`)]

      of vkSet, vkRegular:
        exprNew.add quote do:
          var `vname`: typeof(`typeExpr`)

      of vkAlt:
        if true: raiseAssert("#[ IMPLEMENT ]#")

  # debugecho expr.repr
  updateVarSet(expr, vtable)
  return quote do:
    `exprNew`
    `expr`


macro match*(
  n: tuple | object | ref object | seq | array | set): untyped =
  var matchcase = nnkIfStmt.newTree()

  let trace =
    if debugTraceMatch:
      some ident "trace"
    else:
      none NimNode

  for elem in n[1 .. ^1]:
    case elem.kind:
      of nnkOfBranch:
        if elem[0] == ident "_":
          elem[0].raiseCodeError(
            "To create catch-all match use `else` clause",
            "Replace `_` with `else` here")


        matchcase.add nnkElifBranch.newTree(
          elem[0].parseMatchExpr().makeMatchExpr(trace).
            toNode().newPar().newPar(),
          elem[1]
        )

      of nnkElifBranch, nnkElse:
        matchcase.add elem
      else:
        discard
        # raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")

  let head = n[0]

  let trId =
    if debugTraceMatch:
      quote do:
        var trace {.inject.}: MatchTrace
    else:
      newEmptyNode()

  result = quote do:
    block:
      let expr {.inject.} = `head`
      let pos {.inject.}: int = 0
      `trId`
      `matchcase`

  echov result

macro assertMatch*(input: typed, pattern: untyped): untyped =
  let matched = pattern.parseMatchExpr().
    makeMatchExpr(some ident "trace").toNode()

  return quote do:
    var trace {.inject.}: MatchTrace
    let expr {.inject.} = `input`
    let ok = `matched`
    if not `matched`:
      raiseAssert("Pattern match failed " & $trace)
