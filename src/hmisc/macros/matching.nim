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

template hasKind*(head, kindExpr: untyped): untyped =
  hasKindImpl(head.kind, kindExpr)

type
  EStructKind = enum
    kItem
    kList
    kTuple
    kPairs
    kObject

  ListKeyword = enum
    lkAny ## Any element from list
    lkAll ## All elements from list
    lkNone
    lkOpt
    lkUntil ## All elements until
    lkPos ## Exact position

  ListStructure = object
    bindVar: Option[string]
    patt: Option[PattStructure]
    kind: ListKeyword

  PattStructure = object
    bindVar: Option[string]
    case kind: EStructKind
      of kItem:
        rhs: NimNode
      of kList:
        listElems: seq[ListStructure]
      of kTuple:
        tupleElems: seq[PattStructure]
      of kPairs:
        pairElems: seq[tuple[
          key: NimNode,
          patt: PattStructure
        ]]

      of kObject:
        fldElems: seq[tuple[
          name: string,
          patt: PattStructure
        ]]


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
      of kItem:
        discard

  Path = seq[AccsElem]

  VarSpec = object
    name: string
    decl {.requiresinit.}: NimNode

  Case = object
    expr: NimNode
    heads: seq[NimNode]
    default: Option[NimNode]

func makeVarSpec(name: string, decl: NimNode): VarSpec =
  VarSpec(name: name, decl: decl)

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

    if top.len > 1:
      result = result.aux(top[1 ..^ 1])


  result =
    if path.len > 0:
      (ident name).aux(path)
    else:
      ident name

  # echov result

type
  VarUse = tuple[decl: VarSpec, path: Path]
  ExprRes = tuple[node: NimNode, vars: seq[VarUse]]

func makeMatchExpr(n: NimNode, path: Path): ExprRes =
  # echov path, "Make match expression"
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit:
      if n == ident "_":
        result.node = ident("true")
      else:
        result.node = nnkInfix.newTree(ident "==", path.toAccs("expr"), n)
    of nnkAccQuoted:
      # FIXED should be used instead of `%` prefix
      result.node = nnkInfix.newTree(ident "==", path.toAccs("expr"), n)
    of nnkPar:
      let conds: seq[ExprRes] =
        if n.isNamedTuple():
          collect(newSeq):
            for idx, kv in n:
              if kv.kind == nnkIdent:
                makeMatchExpr(kv, path & @[
                  AccsElem(inStruct: kTuple, idx: newLit(idx))
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
                AccsElem(inStruct: kTuple, idx: newLit(idx))
              ])

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

      # echov result.node

    of nnkPrefix:
      var conds: seq[ExprRes]

      if n[0].strVal() == "==":
        conds.add((
          nnkInfix.newTree(ident "==", path.toAccs("expr"), n[1]), @[]))

      elif n[0].strVal() == "@":
        n[1].assertNodeKind({nnkIdent})
        let
          accs = path.toAccs("expr")
          tmp = n[1].copyNimNode()
          vars = n[1]
          varset = makeVarSet(tmp.strVal(), `accs`)
          node = quote do:
            ((`varset`; true))

        conds = @[(node, @[(makeVarSpec(tmp.strVal(), tmp), path)])]
      elif n[0].strVal() == "in":
        let
          accs = path.toAccs("expr")
          sets = n[1]
          node = quote do:
            (( `accs` in `sets` ))

        conds.add((node, @[]))
      elif n[0].strVal() == "..":
        if not (n[1].kind == nnkIdent and n[1].strVal() == "_"):
          n[1].raiseCodeError("`..` prefix only allowed with `_`")

        n.raiseCodeError("`.._` only allowed in sequence matches")

      else:
        n.raiseCodeError("Unexpected prefix")

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

    of nnkBracket:
      let pos = ident("pos")
      var
        minLen: int = 0
        idx: int = 0
        exactMatches: seq[int]
        elemPath = path & @[AccsElem(inStruct: kList, idx: pos)]

      let
        matched = ident("matched")
        maxPos = ident("maxPos")
        parent = path.toAccs("expr")
        matchBlocks = newStmtList()
        matchBlock = ident("matchBlock")

      func isExactMatch(it: NimNode): bool =
        if it.kind == nnkPrefix:
          it[0].strVal() == "@"
        else:
          true

      for idx, it in n:
        if it.isExactMatch():
          exactMatches.add idx
        else:
          break

      for idx in countdown(n.len - 1, 0):
        if n[idx].isExactMatch():
          if idx notin exactMatches:
            exactMatches.add idx
        else:
          break

      echov n
      while idx < n.len:
        let elem = n[idx]
        if elem.kind == nnkPrefix:
          if idx + 1 < n.len:
            if n[idx + 1].kind == nnkPrefix and
               n[idx + 1][0].strVal() in ["*", "*@", ".."] and
               n[idx][0].strVal() in ["*", "*@", ".."]:
              raise ({
                n[idx + 1] : "Following variadic expression",
                n[idx] : "Variadic expression"
              }).toCodeError(
                "Consecutive variadic expressions are not allowed")

          echov elem
          case elem[0].strVal():
            of "*", "*@", "..":
              let matchExpr =
                case elem[0].strVal():
                  of "*":
                    elem[1]
                  of "*@":
                    elem[1].assertNodeKind({nnkIdent})
                    # result.vars.add(
                    #   makeVarSpec(elem[1].strVal(), elem),
                    #   elemPath.withIt do:
                    #     it[^1].variadicContext = true
                    # )

                    elem.withIt do:
                      echov it[0]
                      it[0] = ident "@"

                  of "..":
                    if elem[1].strVal() != "_":
                      elem.raiseCodeError(
                        "`..` must be followed by underscore `_`")

                    ident("true")
                  else:
                    raiseAssert("#[ IMPLEMENT ]#")


              # echov elem
              # echov matchExpr
              var
                (expr, vars) = matchExpr.makeMatchExpr(
                  elemPath.withIt do:
                    it[^1].variadicContext = true)

                next = if idx + 1 < n.len:
                         inc idx
                         let (subex, vars) = n[idx].makeMatchExpr(
                           elemPath.withIt do:
                             it[^1].variadicContext = true)

                         result.vars.add vars
                         subex
                       else:
                         ident("true")

              result.vars.add vars

              let nextTest =
                if next == ident("true"):
                  quote do:
                    ((matchedNext = true; true))
                else:
                  quote do:
                    not (`next` and (matchedNext = true; true))

              if matchExpr == ident("true"):
                expr = ident("true")

              matchBlocks.add quote do:
                block:
                  var matchedNext {.inject.} = false
                  while (`pos` < `maxPos`) and `nextTest`:
                    # echov `parent`, "@", `pos`
                    if not `expr`:
                      break `matchBlock`
                    else:
                      inc `pos`

                  if matchedNext:
                    inc `pos`
                  else:
                    break `matchBlock`


                  # while (not matchedNext) and (`pos` < `maxPos`):
                  #   inc `pos`
                  #   echov `parent`, "@", `pos`
                  #   if `next`:
                  #     echov "Matched next"
                  #     matchedNext = true
                  #     break
                  #   else:
                  #     dec `pos`




            of "@":
              elem[1].assertNodeKind({nnkIdent})
              let varn = elem[1]
              matchBlocks.add quote do:
                echov `parent`[`pos`]
                varset(`varn`, `parent`[`pos`])
                echov `varn`
                inc `pos`

              result.vars.add(makeVarSpec(elem[1].strVal(), elem), elemPath)
              inc minLen

        else:
          inc minLen
          let (expr, vars) = elem.makeMatchExpr(elemPath)
          matchBlocks.add quote do:
            if not `expr`:
              break `matchBlock`
            else:
              inc `pos`

          result.vars.add vars

        inc idx


      let exactMatch = exactMatches.len == n.len

      let okCond =
        if exactMatch:
          quote do: (`pos` == `minLen`)
        else:
          quote do: (`pos` + 1 >= `minLen`)

      let startCOnd =
        if exactMatch:
          quote do: `maxPos` == `minLen`
        else:
          quote do: `maxPos` >= `minLen`

      result.node = quote do:
        (((
          block:
            var `matched`: bool = false
            let `maxPos` = `parent`.len
            if `startCond`:
              var `pos` = 0
              block `matchBlock`:
                `matchBlocks`
                `matched` = `okCond`

            `matched`
        )))


      # let pattSize = newLit(minLen)
      # var
      #   conds: seq[ExprRes]
      #   idx: int = 0

      # for idx, elem in n:
      #     key = newLit(idx)
      #     (subexp, vars) = elem.makeMatchExpr(path & @[
      #       AccsElem(inStruct: kList, idx: idx)
      #     ])

      #   let node = quote do:
      #     (`parent`.len == `pattSize` and `subexp`)

      #   conds.add(node, vars)

      # result = conds.foldlTuple(
      #   nnkInfix.newTree(ident "and", newPar(a), b)).concatSide()

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
            (`key` in `parent` and `subexp`)

          (node, vars)

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", newPar(a), b)).concatSide()

    of nnkCurly:
      var
        varTrail: VarUse
        captureCount: int = 0 # Number of toplevel captured variables
        conds: seq[ExprRes] # Conditions
        excluded: seq[NimNode]
      let parent = path.toAccs("expr")

      for it in n:

        if it.kind == nnkPrefix and it[0].strVal() == "@":
          # Set variable capture
          if captureCount > 0:
            it.raiseCodeError(
              "Only one toplevel variable capture is allowed for set")
          else:
            varTrail = (makeVarSpec(it[1].strVal(), it[0]), path)
            inc captureCount
        else:
          conds.add nnkInfix.newTree(ident "in", it, parent), @[]
          excluded.add it


      if captureCount > 0:
        conds.add do:
          let
            it = ident "it"
            val = ident "val"
            capture = ident varTrail.decl.name
            noteq = excluded.mapIt(newInfix("!=", val, it)).foldInfix(
              "and", @[ident "true"])

          echov capture
          quote do:
            block:
              for `val` in `parent`:
                if `noteq`:
                  `capture`.incl `val`

              true
        do:
          @[varTrail]

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", newPar(a), b)).concatSide()

    elif n.kind in {nnkObjConstr, nnkCall}:
      let kindCheck = newCall(ident "hasKind", path.toAccs("expr"), n[0])

      var conds: seq[ExprRes]
      conds.add (newCall(ident "hasKind", path.toAccs("expr"), n[0]), @[])

      # echov struct
      for idx, kv in n[1..^1]:
        # echov path
        let parent = path.toAccs("expr")
        echov kv

        if kv.kind == nnkBracket:
          for idx, patt in kv:
            conds.add patt.makeMatchExpr(
              path & @[ AccsElem(inStruct: kList, idx: newLit(idx)) ])
        else:
          conds.add kv[1].makeMatchExpr(
            path & @[ AccsElem(inStruct: kObject, fld: kv[0].strVal()) ])

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", newPar(a), b)).concatSide()

    elif n.isInfixPatt():
      let conds: seq[ExprRes] = collect(newSeq):
        for patt in n[1..^1]:
          patt.makeMatchExpr(path)

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "or", newPar(a), b)).concatSide()

    elif n.kind == nnkInfix and n[0].strVal() == "is":
      echov n
      n[1].assertNodeKind({nnkPrefix})
      n[1][0].assertNodeKind({nnkIdent})

      let name = n[1][1]
      echov name
      let accs = path.toAccs("expr")
      let node = quote do:
        (`name` = `accs`; true)

      var conds: seq[ExprRes]
      conds.add n[2].makeMatchExpr(path)
      conds.add((node, @[(makeVarSpec(name.strVal(), name),path)]))

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", newPar(a), b)).concatSide()
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")

func updateVarSet(nn: NimNode, variadics: seq[string]): void =
  for idx, node in nn:
    if node.kind == nnkCall and
       node[0] == ident "varset":
      echov node
      let
        varn = node[1]
        expr = node[2]

      if varn.strVal() in variadics:
        nn[idx] = quote do:
          `varn`.add `expr`
      else:
        nn[idx] = quote do:
          `varn` = `expr`
    else:
      updateVarSet(nn[idx], variadics)

func makeMatch(n: NimNode, path: Path): NimNode =
  result = nnkIfStmt.newTree()
  for elem in n[1 ..^ 1]:
    case elem.kind:
      of nnkOfBranch:
        let (expr, vars) = makeMatchExpr(elem[0], path)

        var
          variadics: seq[string]

        for varUses in vars.twoPassSortByIt(
          it.decl.name, # Sort by names
          it.decl.decl.lineInfoObj().line + # And by declaration order
          it.decl.decl.lineInfoObj().column * 1000,
        ):
          if varUses.anyOfIt(it.path.isVariadic()):
            variadics.add varUses[0].decl.name
          elif varUses.len > 1:
            raise varUses.mapIt((it.decl.decl, "")).toCodeError(
              &"Multiple uses of binding for `{varUses[0].decl.name}`. " &
                "Only one capture instance is allowed per variable binding"
            )

        var exprNew = nnkStmtList.newTree()
        for v in vars:
          let
            name = ident(v.decl.name)
            typeExpr = toAccs(v.path, "expr")

          if v.decl.name in variadics:
            exprNew.add quote do:
              var `name`: seq[typeof(`typeExpr`)]
          else:
            exprNew.add quote do:
              var `name`: typeof(`typeExpr`)

        exprNew.add expr

        result.add nnkElifBranch.newTree(exprNew, elem[1])
        updateVarSet(result, variadics)
      of nnkElifBranch, nnkElse:
        result.add elem
      else:
        block:
          raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")



macro match*(
  n: tuple | object | ref object | seq | array | set): untyped =
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
      let pos {.inject.}: int = 0
      # let input {.inject.} = `inputExpr`
      # ifHaxComp:
      #   echo typeof(input)

      `matchcase`

  haxThis result.toStrLit()
  # dieHereMacro()
  # assert inputExpr != nil
