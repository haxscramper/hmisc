import sequtils, macros, tables, options, strformat, strutils,
       parseutils, algorithm

template `->`(a, b: bool): bool = (if a: b else: true)

template getSome*[T](opt: Option[T], injected: untyped): bool =
  opt.isSome() and ((let injected {.inject.} = opt.get(); true))

template assertKind*(node: NimNode, kindSet: set[NimNodeKind]): untyped =
  if node.kind notin kindSet:
    raiseAssert("Expected one of " & $kindSet & " but node has kind " &
      $node.kind & "(assertion on " & $instantiationInfo() & ")")

func nodeStr(n: NimNode): string =
  case n.kind:
    of nnkIdent: n.strVal()
    of nnkOpenSymChoice: n[0].strVal()
    else: raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")

func startsWith(n: NimNode, str: string): bool =
  n.nodeStr().startsWith(str)


func idxTreeRepr*(inputNode: NimNode, maxLevel: int = 120): string =
  func aux(node: NimNode, parent: seq[int]): seq[string] =
    result.add parent.mapIt(&"[{it}]").join("") &
      "  ".repeat(6) &
      ($node.kind)[3..^1] &
      (if node.len == 0: " " & node.toStrLit().strVal() else: "")

    for idx, subn in node:
      if parent.len + 1 < maxLevel:
        result &= aux(subn, parent & @[idx])
      else:
        result &= (parent & @[idx]).mapIt(&"[{it}]").join("") &
          " ".repeat(6 + 3 + 3)  & "[...] " & ($subn.kind)[3..^1]

  return aux(inputNode, @[]).join("\n")


func parseEnumField*(fld: NimNode): string =
  ## Get name of enum field from nim node
  case fld.kind:
    of nnkEnumFieldDef:
      fld[0].strVal
    of nnkSym:
      fld.strVal
    else:
      raiseAssert(&"#[ IMPLEMENT {fld.kind} ]#")

func parseEnumImpl*(en: NimNode): seq[string] =
  ## Get sequence of enum value names
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


func pref(name: string): string =
  discard name.parseUntil(result, {'A' .. 'Z', '0' .. '9'})

func newInfix*(s: string, lhs, rhs: NimNode): NimNode =
  ## Create new infix operator `s` with `lhs` and `rhs` as parameters.
  nnkInfix.newTree(ident s, lhs, rhs)

func newPrefix*(s: string, arg: NimNode): NimNode =
  ## Create new prefix operator with argument `arg`
  nnkPrefix.newTree(ident s, arg)

func foldInfix(s: seq[NimNode],
               inf: string, start: seq[NimNode] = @[]): NimNode =
  ( start & s ).mapIt(it.newPar().newPar()).foldl(newInfix(inf, a, b))


func commonPrefix(strs: seq[string]): string =
  ## Find common prefix for list of strings
  if strs.len == 0:
    return ""
  else:
    let strs = strs.sorted()
    for i in 0 ..< min(strs[0].len, strs[^1].len):
      if strs[0][i] == strs[^1][i]:
        result.add strs[0][i]
      else:
        return


func dropPrefix(str: string, alt: string): string =
  if str.startsWith(alt):
    return str[min(alt.len, str.len)..^1]
  return str

func dropPrefix(ss: seq[string], patt: string): seq[string] =
  for s in ss:
    result.add s.dropPrefix(patt)


template findItFirstOpt*(s: typed, op: untyped): untyped =
  var res: Option[typeof(s[0])]
  for it {.inject.} in s:
    if op:
      res = some(it)
      break

  res


func addPrefix*(str, pref: string): string =
  if not str.startsWith(pref): pref & str else: str

macro hasKindImpl*(head: typed, kind: untyped): untyped =
  let
    impl = head.getTypeImpl().parseEnumImpl()
    pref = impl.commonPrefix().pref()
    names = impl.dropPrefix(pref)

  kind.assertKind({nnkIdent, nnkCurly})
  if kind.kind == nnkCurly:
    var idents: seq[NimNode]
    var setadds: seq[NimNode]

    for it in kind:
      if it.kind == nnkIdent:
        idents.add ident(it.toStrLit().strVal().addPrefix(pref))
      elif it.kind == nnkPrefix:
        it[1].assertKind({nnkIdent})
        setadds.add it[1]

    setadds.add nnkCurly.newTree(idents)

    result = newInfix("in", head, newPar(setadds.foldInfix("+")))

  else:
    let kind = ident(kind.toStrLit().strVal().addPrefix(pref))
    result = nnkInfix.newTree(ident "==", head, kind)


template hasKind*(head, kindExpr: untyped): untyped =
  ## Determine if `head` has `kind` value. Either function/procedure
  ## `kind` or field with the same name is expecte to be declared.
  ## Type of `kind` must be an enum. Kind expression is a pattern
  ## describing expected values. Possible examples of pattern
  ## (assuming value of type `NimNode` is used as `head`)
  ##
  ## - `nnkIntLit` - match integer literal
  ## - `IntLit` - alternative (preferred) syntax for matching enum values
  ##   `nnk` prefix can be omitted.
  ## - `{IntLit, StrLit}` - check for multiple kinds at the same time
  ## - `{IntLit, +StrLiterals}` - check if kind value is either in an
  ##   integer literal or is contained in set `StrLiterals` (which must)
  ##   be declared externally. THis syntax is useful for checking common
  ##   sets of kind values such as 'integer literals', 'literals' etc.
  ##
  ## NOTE: this template is used internally by `match` macro
  ## implementation - all patterns can also be used to match case
  ## objects (for example `{IntLit, StrLit}()` to match either integer
  ## or string literal node or `{+Literals}()` for matching any
  ## literal node)
  hasKindImpl(head.kind, kindExpr)

type
  MatchKind* = enum
    ## Different kinds of matching patterns
    kItem ## Match single element
    kList ## Match sequence of elements
    kTuple ## Mach tuple (anonymous or named)
    kPairs ## Match key-value pairs
    kObject ## Match object, named tuple or object-like value
    kSet ## Match set of elements
    kAlt ## Ordered choice - mactch any of patterns.

  ListKeyword* = enum
    ## Possible special words for list pattern matching
    lkAny = "any" ## Any element from list
    lkAll = "all" ## All elements from list
    lkNone = "none"  ## None of the elements from list
    lkOpt = "opt" ## Optionaly match element in list
    lkUntil = "until" ## All elements until
    lkPref = "pref" ## All elements while
    lkPos ## Exact position
    lkSlice ## Subrange slice
    lkTrail ## Variadic placeholder `.._`

  ListStructure* = object
    decl: NimNode ## Original declaration of the node
    bindVar*: Option[NimNode] ## Optional bound variable
    patt*: Match
    case kind*: ListKeyword
      of lkSlice:
        slice*: NimNode
      else:
        discard

  ItemMatchKind* = enum
    ## Type of item pattern match
    imkInfixEq ## Match item using infix operator
    imkSubpatt ## Match item by checking it agains subpattern
    imkPredicate ## Execute custom predicate to determine if element
                 ## matches pattern.

  KVPair* = tuple[key: NimNode, patt: Match]
  Match* = ref object
    ## Object describing single match for element
    bindVar*: Option[NimNode] ## Bound variable (if any)
    declNode: NimNode ## Original declaration of match
    isOptional: bool
    case kind*: MatchKind
      of kItem:
        case itemMatch: ItemMatchKind
          of imkInfixEq:
            infix*: string ## Infix operator used for comparison
            rhsNode*: NimNode ## Rhs expression to compare against
            isPlaceholder*: bool ## Always true? `_` pattern is an
            ## infix expression with `isPlaceholder` equal to true
          of imkSubpatt:
            rhsPatt*: Match ## Subpattern to compare value against
          of imkPredicate:
            isCall*: bool ## Predicate is a call expression
            ## (`@val.matches()`) or a free-standing expression
            ## (`@val(it.len < 100)`)
            predBody*: NimNode ## Body of the expression

      of kAlt:
        altElems*: seq[Match] ## Alternatives for matching
      of kList:
        listElems*: seq[ListStructure] ## Sequence subpatterns
      of kTuple:
        tupleElems*: seq[Match] ## Tuple elements
      of kPairs:
        pairElems*: seq[KVPair]

      of kSet:
        setElems*: seq[Match]
      of kObject:
        kindCall*: Option[NimNode] ## Optional node with kind
        ## expression pattern (see `hasKind`)
        fldElems*: seq[tuple[
          name: string,
          patt: Match
        ]]

        kvMatches*: Option[Match] ## Optional key-value matches for
        ## expressions like `JObject({"key": @val})`
        listMatches*: Option[Match]  ## Optional indexed matches for
        ## subelement access using `Infix([@op, @lhs, @rhs])` pattern.

  AccsElem = object
    isVariadic: bool
    case inStruct: MatchKind
      of kList:
        pos: NimNode
      of kTuple:
        idx: int
      of kObject:
        fld: string
      of kPairs:
        parentKey: bool
        key: NimNode
        nocheck: bool
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
    decl: NimNode
    varKind: VarKind
    typePath: Path

  VarTable = Table[string, VarSpec]

func isNamedTuple(node: NimNode): bool =
  node.allIt(it.kind in {
    nnkExprColonExpr, # `(fld: )`
    nnkBracket, # `([])`
    nnkTableConstr # `{key: val}`
  }) and
  node.allIt((it.kind == nnkIdent) -> (it.strVal == "_"))

func isInfixPatt(node: NimNode): bool =
  node.kind == nnkInfix and
  node[0].kind == nnkIdent and
  node[0].strVal() in ["|"]

func makeVarSet(v: NimNode, expr: NimNode): NimNode =
  v.assertKind({nnkIdent})
  newCall(ident "varset", v, expr)

func toAccs*(path: Path, name: string): NimNode =
  ## Convert path in object to expression for getting element at path.
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


func parseMatchExpr*(n: NimNode): Match

func parseKVTuple(n: NimNode): Match =
  if n[0].eqIdent("Some"):
    if not (n.len <= 2):
      error("Expected `Some(@varBind)`", n)

    n[1].assertKind({nnkPrefix})
    n[1][0].assertKind({nnkIdent})

    return Match(kind: kObject, declNode: n, fldElems: @{
      "isSome": Match(kind: kItem, itemMatch: imkInfixEq, declNode: n[0],
                      rhsNode: newLit(true), infix: "=="),
      "get": Match(kind: kItem, itemMatch: imkInfixEq,
                   declNode: n[1], isPlaceholder: true,
                   bindVar: some(n[1][1])),
    })


  result = Match(kind: kObject, declNode: n)
  var start = 0
  if n.kind in {nnkCall, nnkObjConstr}:
    start = 1
    result.kindCall = some(n[0])

  for elem in n[start .. ^1]:
    case elem.kind:
      of nnkExprColonExpr:
        elem[0].assertKind({nnkIdent})
        result.fldElems.add((
          elem[0].strVal(),
          elem[1].parseMatchExpr()))
      of nnkBracket:
        result.listMatches = some(elem.parseMatchExpr())
      of nnkTableConstr:
        result.kvMatches = some(elem.parseMatchExpr())
      else:
        elem.assertKind({nnkExprColonExpr})

func contains(kwds: openarray[ListKeyword], str: string): bool =
  for kwd in kwds:
    if eqIdent($kwd, str):
      return true

func parseListMatch(n: NimNode): seq[ListStructure] =
  for elem in n:
    if elem.kind == nnkPrefix and elem[0].eqIdent(".."):
      elem[1].assertKind({nnkIdent})
      result.add ListStructure(kind: lkTrail, patt: Match(
        declNode: elem
      ))
    elif
      # `[0 .. 3 @head is Jstring()]`
      (elem.kind == nnkInfix and (elem[0].startsWith(".."))) or
      # `[(0 .. 3) @head is Jstring()]`
      (elem.kind == nnkCommand and elem[0].kind == nnkPar) or
      # `[0 .. 2 is 12]`
      (elem.kind == nnkInfix and
       elem[1].kind == nnkInfix and
       elem[1][0].startsWith("..")
      )
      :
      var dotInfix, rangeStart, rangeEnd, body: NimNode
      if elem.kind == nnkInfix:
        if elem.kind == nnkInfix and elem[1].kind == nnkInfix:
          # `0 .. 2 is 12`
          #             Infix
          # [0]            Ident is
          # [1]            Infix
          # [1][0]            [...] Ident
          # [1][1]            [...] IntLit
          # [1][2]            [...] IntLit
          # [2]            IntLit 12
          dotInfix = elem[1][0]
          rangeStart = elem[1][1]
          rangeEnd = elem[1][2]
          body = elem[2]
        else:
          # `0 .. 2 @a is 12`
          #             Infix
          # [0]            Ident ..
          # [1]            IntLit 0
          # [2]            Command
          # [2][0]            IntLit 2
          # [2][1]            Infix
          # [2][1][0]            [...] Ident
          # [2][1][1]            [...] Prefix
          # [2][1][2]            [...] IntLit
          dotInfix = ident elem[0].nodeStr()
          rangeStart = elem[1]
          rangeEnd = elem[2][0]
          body = elem[2][1]

      elif elem.kind == nnkCommand:
        # I wonder, why do we need pattern matching in stdlib?
        dotInfix = ident elem[0][0][0].nodeStr()
        rangeStart = elem[0][0][1]
        rangeEnd = elem[0][0][1]
        body = elem[1]

      var res = ListStructure(
        kind: lkSlice, slice: nnkInfix.newTree(
          dotInfix,
          rangeStart,
          rangeEnd
        ),
        patt: parseMatchExpr(body)
      )

      res.bindVar = res.patt.bindVar
      res.patt.bindVar = none(NimNode)
      result.add res

      # debugecho elem.treeRepr()
    else:
      var (elem, opKind) = (elem, lkPos)
      if elem.kind in {nnkCall, nnkCommand} and
         elem[0].kind notin {nnkDotExpr} and
         elem[0].strVal() in [
        lkAny, lkAll, lkNone, lkOpt, lkUntil, lkPref]:
        var kwd: ListKeyword
        for (key, val) in {
          "any" : lkAny,
          "all" : lkAll,
          "opt" : lkOpt,
          "until" : lkUntil,
          "none" : lkNone,
          "pref" : lkPref
            }:
          if elem[0].eqIdent(key):
            kwd = val
            break


        elem = elem[1]
        opKind = kwd

      var
        match = parseMatchExpr(elem)
        bindv = match.bindVar

      match.bindVar = none(NimNode)
      match.isOptional = opKind in {lkOpt}

      var it = ListStructure(bindVar: bindv, kind: opKind)
      it.patt = match
      result.add(it)

func parseTableMatch(n: NimNode): seq[KVPair] =
  for elem in n:
    result.add((elem[0], elem[1].parseMatchExpr()))

func parseAltMatch(n: NimNode): Match =
  let
    lhs = n[1].parseMatchExpr()
    rhs = n[2].parseMatchExpr()

  var alts: seq[Match]
  if lhs.kind == kAlt: alts.add lhs.altElems else: alts.add lhs
  if rhs.kind == kAlt: alts.add rhs.altElems else: alts.add rhs
  result = Match(kind: kAlt, altElems: alts, declNode: n)

func parseMatchExpr*(n: NimNode): Match =
  ## Parse match expression from nim node
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit, nnkCharLit:
      result = Match(kind: kItem, itemMatch: imkInfixEq, declNode: n)
      if n == ident "_":
        result.isPlaceholder = true
      else:
        result.rhsNode = n
        result.infix = "=="
    of nnkPar: # Named or unnamed tuple
      if n.isNamedTuple(): # `(fld1: ...)`
        result = parseKVTuple(n)
      elif n[0].isInfixPatt(): # `(12 | 3)`
        result = parseAltMatch(n[0])
      else: # Unnamed tuple `( , , , , )`
        result = Match(kind: kTuple, declNode: n)
        for elem in n:
          result.tupleElems.add parseMatchExpr(elem)
    of nnkPrefix: # `is Patt()`, `@capture` or other prefix expression
      if n[0].nodeStr() == "is": # `is Patt()`
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[1]), declNode: n)

      elif n[0].nodeStr() == "@": # `@capture`
        n[1].assertKind({nnkIdent})
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, isPlaceholder: true,
          bindVar: some(n[1]), declNode: n)

      else: # Other prefix expression, for example `== 12`
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, infix: n[0].strVal(),
          rhsNode: n[1], declNode: n
        )

    of nnkBracket: # `[1,2,3]` - list pattern
      result = Match(
        kind: kList, listElems: parseListMatch(n), declNode: n)
    of nnkTableConstr: # `{"key": "val"}` - key-value matches
      result = Match(
        kind: kPairs, pairElems: parseTableMatch(n), declNode: n)
    of nnkCurly: # `{1, 2}` - set pattern
      result = Match(kind: kSet, declNode: n)
      for node in n:
        if node.kind in {nnkExprColonExpr}:
          error("Unexpected colon", node) # TODO:DOC


        result.setElems.add parseMatchExpr(node)
    of nnkObjConstr, nnkCall:
      if n[0].kind == nnkPrefix:
        n[0][1].assertKind({nnkIdent}) # `@capture(<some-expression>)`
        result = Match(
          kind: kItem,
          itemMatch: imkPredicate,
          bindVar: some(n[0][1]),
          declNode: n,
          predBody: n[1]
        )
      elif n[0].kind == nnkDotExpr: # `_.call("Arguments")`
        # `(DotExpr (Ident "_") (Ident "<function-name>"))`
        n[0][1].assertKind({nnkIdent})
        n[0][0].assertKind({nnkIdent})
        var body = n
        # Replace `_` with `it` to make `it.call("arguments")`
        body[0][0] = ident("it")
        result = Match(
          kind: kItem,
          itemMatch: imkPredicate,
          declNode: n,
          predBody: body
        )
      else:
        result = parseKVTuple(n)
    elif n.isInfixPatt():
      result = parseAltMatch(n)
    elif n.kind == nnkInfix:
      n[1].assertKind({nnkPrefix})
      n[1][1].assertKind({nnkIdent})
      if n[0].strVal() == "is":
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[2]), declNode: n)

      else:
        result = Match(
          kind: kItem, itemMatch: imkInfixEq,
          rhsNode: n[2],
          infix: n[0].strVal(), declNode: n)

        if result.infix == "or":
          result.isOptional = true

      result.bindVar = some(n[1][1])
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")

func isVariadic(p: Path): bool = p.anyIt(it.isVariadic)

func isAlt(p: Path): bool = p.anyIt(it.inStruct == kAlt)

func isOption(p: Path): bool =
  p.anyIt(it.inStruct == kItem and it.isOpt)

func classifyPath(path: Path): VarKind =
  if path.isVariadic:
    vkSequence
  elif path.isAlt:
    vkAlt
  elif path.isOption:
    vkOption
  else:
    vkRegular

func addvar(tbl: var VarTable, vsym: NimNode, path: Path): void =
  let vs = vsym.strVal()
  if vs notin tbl:
    tbl[vs] = VarSpec(
      decl: vsym,
      varKind: path.classifyPath(),
      typePath: path
    )
  else:
    let class = path.classifyPath()
    var update = false
    case class:
      of vkSequence:
        update = true

      of vkOption:
        if tbl[vs].varKind in {vkRegular}:
          update = true

      else:
        discard

    if update:
      tbl[vs].varKind = class
      tbl[vs].typePath = path

func makeMatchExpr*(
  m: Match, vt: var VarTable, path: Path, mainExpr: string): NimNode

template makeElemMatch(): untyped {.dirty.} =
  case elem.kind:
    of lkPos:
      inc minLen
      inc maxLen
      if elem.bindVar.getSome(bindv):
        result.add makeVarSet(bindv, parent.toAccs(mainExpr))
        vt.addvar(bindv, parent)

      if elem.patt.kind == kItem and
         elem.patt.itemMatch == imkInfixEq and
         elem.patt.isPlaceholder:
        result.add newCall(ident "inc", posid)
      else:
        result.add quote do:
          if `expr`:
            inc `posid`
          else:
            `failBreak`

    else:
      maxLen = 5000
      var varset = newEmptyNode()

      if elem.bindVar.getSome(bindv):
        varset = makeVarSet(bindv, parent.toAccs(mainExpr))
        vt.addvar(bindv, parent)

      case elem.kind:
        of lkAll:
          result.add quote do:
            block:
              var allOk: bool = true
              while `posid` < `getLen` and allOk:
                if not `expr`:
                  allOk = false
                else:
                  `varset`
                  inc `posid`

              if not allOk:
                break `failBlock`

        of lkSlice:
          var rangeExpr = elem.slice
          result.add quote do:
            for tmp in `rangeExpr`:
              `posid` = tmp
              if `posid` < `getLen` and `expr`:
                `varset`
              else:
                break `failBlock`

        of lkUntil:
          result.add quote do:
            while (`posid` < `getLen`) and (not `expr`):
              `varset`
              inc `posid`

          if idx == list.listElems.len - 1:
            result.add quote do:
              if (`posid` < `getLen`): ## Not full match
                break `failBlock`

        of lkAny:
          result.add quote do:
            block:
              var foundOk: bool = false
              while `posid` < `getLen`:
                if `expr`:
                  foundOk = true
                  `varset`
                  inc `posid`

              if not foundOk:
                break `failBlock`
        of lkPref:
          result.add quote do:
            while `posid` < `getLen` and `expr`:
              `varset`
              inc `posid`
        of lkOpt:
          var default = nnkDiscardStmt.newTree(newEmptyNode())
          if elem.patt.isOptional:
            if elem.bindVar.getSome(bindv):
              if elem.patt.rhsNode != nil:
                default = makeVarSet(bindv, elem.patt.rhsNode)
                vt.addvar(bindv, path & @[
                  AccsElem(inStruct: kList, pos: posid),
                  AccsElem(inStruct: kItem)
                ])
              else:
                vt.addvar(bindv, path & @[
                  AccsElem(inStruct: kList, pos: posid),
                  AccsElem(inStruct: kItem, isOpt: true)
                ])

          result.add quote do:
            if `posid` < `getLen`:
              `varset`
              inc `posid`
            else:
              `default`
        else:
          if true:
            raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")



func makeListMatch(
  list: Match, vt: var VarTable, path: Path,
  mainExpr: string): NimNode =
  var idx = 1
  while idx < list.listElems.len:
    if list.listElems[idx - 1].kind notin {lkUntil, lkPos, lkOpt, lkPref}:
      error("Greedy list match must be last element in pattern",
            list.listElems[idx].decl)

    inc idx

  let
    posid = genSym(nskVar, "pos")
    matched = genSym(nskVar, "matched")
    failBlock = ident("failBlock")
    failBreak = nnkBreakStmt.newTree(failBlock)
    getLen = newCall("len", path.toAccs(mainExpr))


  result = newStmtList()
  var minLen = 0
  var maxLen = 0
  for idx, elem in list.listElems:
    if elem.kind == lkTrail:
      maxLen = 5000
    else:
      let
        parent = path & @[AccsElem(
          inStruct: kList, pos: posid,
          isVariadic: elem.kind notin {lkPos, lkOpt})]

        expr = elem.patt.makeMatchExpr(vt, parent, mainExpr)


      result.add newCommentStmtNode(
        $elem.kind & " " & elem.patt.declNode.repr)

      makeElemMatch()

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

  result = quote do:
    `comment`
    var `matched` = false
    block `failBlock`:
      var `posid` = 0 ## Start list match

      if `setCheck`:
        ## fail on seq len
        break `failBlock`

      `result`

      `matched` = true ## List match ok

    `matched`

  result = result.newPar().newPar()




func makeMatchExpr*(
  m: Match, vt: var VarTable, path: Path, mainExpr: string): NimNode =
  ## Create NimNode for checking whether or not item referred to by
  ## `mainExpr` matches pattern described by `Match`
  case m.kind:
    of kItem:
      let parent = path.toAccs(mainExpr)
      case m.itemMatch:
        of imkInfixEq, imkSubpatt:
          let inf =
            if m.itemMatch == imkInfixEq:
              if m.isPlaceholder:
                newLit(true)
              else:
                newInfix(m.infix, parent, m.rhsNode)
             else:
               makeMatchExpr(m.rhsPatt, vt, path, mainExpr)

          if m.bindVar.getSome(vname):
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
        of imkPredicate:
          let pred = m.predBody
          var bindVar = newEmptyNode()
          if m.bindVar.getSome(vname):
            vt.addvar(vname, path)
            bindVar = makeVarSet(vname, parent)

          result = quote do:
            let it {.inject.} = `parent`
            if `pred`:
              `bindVar`
              true
            else:
              false


    of kList:
      return makeListMatch(m, vt, path, mainExpr)
    of kTuple:
      var conds: seq[NimNode]
      for idx, it in m.tupleElems:
        conds.add it.makeMatchExpr(vt, path & @[
          AccsElem(inStruct: kTuple, idx: idx)
        ],  mainExpr)

      return conds.foldInfix("and")
    of kObject:
      var conds: seq[NimNode]
      for (fld, patt) in m.fldElems:
        conds.add patt.makeMatchExpr(vt, path & @[
          AccsElem(inStruct: kObject, fld: fld)],  mainExpr)

      if m.listMatches.getSome(list):
        conds.add list.makeMatchExpr(vt, path,  mainExpr)

      if m.kvMatches.getSome(kv):
        conds.add kv.makeMatchExpr(vt, path,  mainExpr)

      if m.kindCall.getSome(kc):
        conds.add newCall(ident "hasKind", path.toAccs(mainExpr), kc)

      return conds.foldInfix("and")

    of kPairs:
      var conds: seq[NimNode]
      for (key, val) in m.pairElems:
        conds.add newInfix(
          "and",
          newInfix("in", key, path.toAccs(mainExpr)),
          val.makeMatchExpr(vt, path & @[
            AccsElem(inStruct: kPairs, key: key)],  mainExpr))

      return conds.foldInfix("and")
    of kAlt:
      var conds: seq[NimNode]
      for alt in m.altElems:
        conds.add alt.makeMatchExpr(
          vt, path & @[AccsElem(inStruct: kAlt)],  mainExpr)

      return conds.foldInfix("or")
    of kSet:
      var conds: seq[NimNode]
      let setPath = path.toAccs(mainExpr)
      for elem in m.setElems:
        if elem.kind == kItem and elem.infix == "==":
          conds.add newInfix("in", elem.rhsNode, setPath)
        else:
          error(
            "Only `contains` check are supported for sets",
            elem.declNode
          )

      return conds.foldInfix("and")


func makeMatchExpr(m: Match, mainExpr: string): tuple[
    expr: NimNode, vtable: VarTable] =
  result.expr = makeMatchExpr(m, result.vtable, @[],  mainExpr)

func updateTypeof(nn: NimNode): void =
  for idx, node in nn:
    if node.kind == nnkSym and node.strVal.endsWith("pos"):
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

        of vkRegular, vkAlt:
          nn[idx] = nnkAsgn.newTree(varn, expr)

    elif node.kind == nnkIfStmt and
         node[0][0].kind in {nnkSym, nnkIdent} and
         node[0][0].strVal == "true":
        nn[idx] = nn[idx][0][1]
        updateVarSet(nn[idx], vtable)
    else:
      updateVarSet(nn[idx], vtable)

func toNode(
  input: tuple[expr: NimNode, vtable: VarTable], mainExpr: string): NimNode =
  var (expr, vtable) = input

  var exprNew = nnkStmtList.newTree()
  for name, spec in vtable:
    let vname = ident(name)
    var typeExpr = toAccs(spec.typePath, mainExpr)
    typeExpr = quote do:
      ((let tmp = `typeExpr`; tmp))

    updateTypeof(typeExpr)
    case spec.varKind:
      of vkSequence:
        exprNew.add quote do:
          var `vname`: seq[typeof(`typeExpr`)]

      of vkOption:
        exprNew.add quote do:
          var `vname`: Option[typeof(`typeExpr`)]

      of vkSet, vkRegular, vkAlt:
        exprNew.add quote do:
          var `vname`: typeof(`typeExpr`)

  updateVarSet(expr, vtable)
  return quote do:
    `exprNew`
    `expr`

macro expand*(body: typed): untyped = body

macro match*(n: untyped): untyped =
  var matchcase = nnkIfStmt.newTree()
  for elem in n[1 .. ^1]:
    case elem.kind:
      of nnkOfBranch:
        if elem[0] == ident "_":
          error("To create catch-all match use `else` clause", elem[0])


        matchcase.add nnkElifBranch.newTree(
          elem[0].parseMatchExpr().makeMatchExpr( "expr").
            toNode("expr").newPar().newPar(),
          elem[1]
        )

      of nnkElifBranch, nnkElse:
        matchcase.add elem
      else:
        discard

  let head = n[0]

  let pos = newCommentStmtNode($n.lineInfoObj())

  result = quote do:
    block:
      `pos`
      let expr {.inject.} = `head`
      let pos {.inject.}: int = 0
      `matchcase`

macro assertMatch*(input, pattern: untyped): untyped =
  let
    expr = ident genSym(nskLet, "expr").repr
    matched = pattern.parseMatchExpr().
      makeMatchExpr(expr.repr).toNode(expr.repr)


  let patt = newLit(pattern.repr)
  result = quote do:
    let `expr` = `input`
    let ok = `matched`

    if not ok:
      raiseAssert("Pattern match failed `" & `patt` & "`")

macro matches*(input, pattern: untyped): untyped =
  let
    expr = ident genSym(nskLet, "expr").repr
    matched = pattern.parseMatchExpr().
      makeMatchExpr(expr.repr).toNode(expr.repr)

  return quote do:
    let `expr` = `input`
    `matched`

template `:=`*(lhs, rhs: untyped): untyped =
  assertMatch(rhs, lhs)

template `?=`*(lhs, rhs: untyped): untyped =
  matches(rhs, lhs)
