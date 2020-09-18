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

  EStruct = ref object
    case kind: EStructKind
      of kItem:
        canOverride: bool
      of kList:
        item: EStruct
      of kPairs:
        key: EStruct
        value: EStruct
      of kTuple:
        elements: seq[EStruct]
      of kObject:
        isKind: bool
        flds: seq[tuple[
          name: string,
          struct: EStruct
        ]]

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

func `$`(es: EStruct): string =
  if es.isNil:
    "nil"
  else:
    case es.kind:
      of kList: &"[{es.item}]"
      of kItem: "*"
      of kTuple: es.elements.mapIt($it).join(", ").wrap("()")
      of kPairs: &"{{{es.key}: {es.value}}}"
      of kObject: es.flds.
        mapPairs(&"{lhs}: {rhs}").
        join(", ").wrap(("#(", ")"))


func isNamedTuple(node: NimNode): bool =
  node.allOfIt(it.kind in {nnkExprColonExpr, nnkIdent}) and
  node.allOfIt((it.kind == nnkIdent) -> it.strVal == "_")

# func isKindCall(node: NimNode): bool =
#   node[0].kind == nnkIdent and node[0].strVal()[0].isUpperAscii()

func isInfixPatt(node: NimNode): bool =
  node.kind == nnkInfix and node[0].strVal() in ["|"]

func makeExpected(node: NimNode): EStruct =
  # echov node
  case node.kind:
    of nnkCurly:
      return EStruct(kind: kPairs)
    of nnkPar:
      if node.isNamedTuple():
        return EStruct(kind: kObject)
      elif node.noneOfIt(it.kind in {nnkExprColonExpr}):
        return EStruct(kind: kTuple)
      else:
        node.raiseCodeError(
          "Mix of named and unnamed fields is not allowed")

    of nnkIdent, nnkIntLit, nnkInfix, nnkStrLit:
      if node.isInfixPatt():
        # NOTE assuming only one type of objects will be matched in
        # infix alternative. Some limited support for things like `12
        # | "hello"` is present because such things are all mapped to
        # `kItem`
        return makeExpected(node[1])
      else:
        result = EStruct(kind: kItem)
        if node.kind == nnkIdent and node.strVal() == "_":
          result.canOverride = true

    of nnkCall, nnkObjConstr:
      return EStruct(kind: kObject)
    of nnkTableConstr:
      return EStruct(kind: kPairs)
    of nnkBracket:
      return EStruct(kind: kList)
    of nnkPrefix:
      case node[0].strVal():
        of "@":
          assertNodeKind(node[1], {nnkBracket})
          return EStruct(kind: kList)
        of "$":
          return EStruct(kind: kItem, canOverride: true)
        else:
          node.raiseCodeError("Unexpected prefix")
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {node.kind} ]#")

func updateExpected(
  parent: var EStruct, node: NimNode, path: Path): void =
  assert not parent.isNil
  case node.kind:
    of nnkTableConstr:
      assert parent.kind == kPairs
      for kv in node:
        assertNodeKind(kv, {nnkExprColonExpr})
        if parent.key.isNil:
          parent.key = makeExpected(kv[0])

        if parent.value.isNil:
          parent.value = makeExpected(kv[1])

        parent.key.updateExpected(kv[0], path & @[
          AccsElem(inStruct: kPairs, parentKey: true, key: kv[0])
        ])

        parent.value.updateExpected(kv[1], path & @[
          AccsElem(inStruct: kPairs, parentKey: false, key: kv[1])
        ])


    of nnkCurly:
      assert parent.kind == kPairs
      for idx, subn in node:
        assertNodeKind(subn, {nnkExprColonExpr})
        parent.key.updateExpected(subn[0], path & @[
          AccsElem(inStruct: kPairs, parentKey: true)])
        parent.value.updateExpected(subn[1], path & @[
          AccsElem(inStruct: kPairs, parentKey: true)])
    of nnkPar, nnkObjConstr:
      case parent.kind:
        of kObject:
          if node.kind == nnkObjConstr:
            parent.isKind = true

          for idx, subn in node:
            if subn.kind notin {nnkIdent}:
              assertNodeKind(subn[0], {nnkIdent}) # TODO nnkAccQuoted
              let fld = subn[0].strVal()
              var idx = parent.flds.findIt(it.name == fld)
              if idx == -1:
                parent.flds.add (name: fld, struct: makeExpected(subn[1]))
                idx = parent.flds.len - 1

              parent.flds[idx].struct.updateExpected(subn[1], path & @[
                AccsElem(inStruct: kObject, fld: fld)
              ])

        of kTuple:
          for idx, subn in node:
            if parent.elements.len <= idx:
              assertNodeKindNot(subn, {nnkExprColonExpr})
              # echov subn, "Created new subnode for parent", parent
              parent.elements.add makeExpected(subn)
              # echov parent
              # echov parent

            parent.elements[idx].updateExpected(subn, path & @[
              AccsElem(inStruct: kTuple, idx: idx)])
        of kItem:
          if parent.canOverride:
            # echov node, "Overriding parent from", node
            parent = makeExpected(node)
            parent.updateExpected(node, path)
            # echov parent

        else:
          raiseAssert(&"#[ IMPLEMENT {parent.kind} ]#")

    of nnkPrefix:
      discard

    of nnkIdent, nnkIntLit, nnkInfix, nnkStrLit, nnkCall:
      if node.kind == nnkCall:
        parent.isKind = true

      if node.isInfixPatt():
        for subn in node[1..^1]:
          parent.updateExpected(subn, path)

    of nnkBracket:
      assert parent.kind == kList
      for idx, subn in node:
        if parent.item.isNil:
          parent.item = makeExpected(subn)

        parent.item.updateExpected(subn, path & @[
          AccsElem(inStruct: kList)
        ])

    else:
      raiseAssert(&"#[ IMPLEMENT for kind {node.kind} ]#")

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

func makeInput(top: EStruct, path: Path): NimNode =
  case top.kind:
    of kItem:
      return path.toAccs("expr")
    of kTuple:
      return newPar: collect(newSeq):
        for idx, subn in top.elements:
          subn.makeInput(path & @[
            AccsElem(inStruct: kTuple, idx: idx)
          ])
    of kObject:
      let flds = collect(newSeq):
        for (name, struct) in top.flds:
          newColonExpr(ident name, struct.makeInput(path & @[
            AccsElem(inStruct: kObject, fld: name)
          ]))

      if top.isKind:
        return newPar: flds & @[
          newColonExpr(ident "kind",
                       newDotExpr(path.toAccs("expr"), ident "kind"))
        ]
      else:
        return newPar(flds)

    of kPairs:
      return path.toAccs("expr")
    of kList:
      return newCall("toSeq", path.toAccs("expr"))

type ExprRes = tuple[
  node: NimNode, vars: seq[tuple[name: string, path: Path]]]

func makeMatchExpr(n: NimNode, path: Path, struct: EStruct): ExprRes =
  # echov path, "Make match expression"
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit:
      if n == ident "_":
        result.node = ident("true")
      else:
        result.node = nnkInfix.newTree(ident "==", path.toAccs("input"), n)
    of nnkPar:
      let conds: seq[ExprRes] =
        if n.isNamedTuple():
          collect(newSeq):
            for idx, kv in n:
              if kv.kind == nnkIdent:
                makeMatchExpr(kv, path & @[
                  AccsElem(inStruct: kTuple, idx: idx)
                ], struct.flds[idx].struct)
              else:
                echov kv[1], kv[0].strVal()
                let val = makeMatchExpr(kv[1], path & @[
                  AccsElem(inStruct: kObject, fld: kv[0].strVal())
                ],
                  struct.flds.findItFirst(it.name == kv[0].strVal()).struct
                )

                val
        else:
          collect(newSeq):
            for idx, val in n:
              makeMatchExpr(val, path & @[
                AccsElem(inStruct: kTuple, idx: idx)
              ], struct.elements[idx])

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

      # echov result.node

    of nnkPrefix:
      let conds: seq[ExprRes] =
        if n[0].strVal() == "$":
          let
            accs = path.toAccs("input")
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
              parent = path.toAccs("input")
              key = newLit(idx)
              (subexp, vars) = elem.makeMatchExpr(path & @[
                AccsElem(inStruct: kList, idx: idx)
              ], struct.item)

              input = struct.item.makeInput(@[])

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
            parent = path.toAccs("input")
            key = kv[0]
            (subexp, vars) = kv[1].makeMatchExpr(@[
                AccsElem(inStruct: kPairs, key: kv[0])
            ], struct.value)
            input = struct.value.makeInput(@[])


          let node = quote do:
            ((
              block:
                if `key` in `parent`:
                  let expr {.inject.} = `parent`[`key`]
                  `subexp`
                else:
                  false
            ))

          (node, vars)

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()


    elif n.kind in {nnkObjConstr, nnkCall}:
      let kindCheck = newCall(ident "hasKind", path.toAccs("input"), n[0])
      if struct.kind == kObject:
        iflet (errn = n[1..^1].findItFirstOpt(it.kind notin {
          nnkExprColonExpr, nnkBracket})):
          errn.raiseCodeError(
            "Only `fld: <patt>` or `[<items>]` matches can be used in object")


      var conds: seq[ExprRes]
      conds.add (newCall(ident "hasKind", path.toAccs("input"), n[0]), @[])

      # echov struct
      for idx, kv in n[1..^1]:
        # echov path
        let parent = path.toAccs("input")

        let tmp = kv[1].makeMatchExpr(path & @[
          AccsElem(
            inStruct: kObject,
            fld:
              if kv.kind == nnkBracket:
                "items"
              else:
                kv[0].strVal()
          )
        ],
          struct.flds.findItFirst(it.name == kv[0].strVal()).struct
          # if kv.kind == nnkBracket:
          #   "items"
          # else:
            # kv[0].strVal()
        )

        # echov parent
        # echov kv
        # echov tmp.node

        conds.add tmp


      result = conds.foldlTuple(
        nnkInfix.newTree(ident "and", a, b)).concatSide()

      # echov result.node

    elif n.isInfixPatt():
      let conds: seq[ExprRes] = collect(newSeq):
        for patt in n[1..^1]:
          patt.makeMatchExpr(path, struct)

      result = conds.foldlTuple(
        nnkInfix.newTree(ident "or", a, b)).concatSide()
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")



func makeMatch(n: NimNode, path: Path, top: EStruct): NimNode =
  result = nnkIfStmt.newTree()
  for elem in n[1 ..^ 1]:
    case elem.kind:
      of nnkOfBranch:
        let (expr, vars) = makeMatchExpr(elem[0], path, top)

        let varDecsl = vars.deduplicateIt(it.name)
        var exprNew = nnkStmtList.newTree()
        for v in vars:
          let
            name = ident(v.name)
            typeExpr = toAccs(v.path, "input")

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

  # Determine kind of expected toplevel structure
  var toplevel: EStruct
  for head in n[1 ..^  1]:
    if head.kind == nnkOfBranch:
      let head = head[0]
      if head == ident "_":
        raiseCodeError(head,
                       "To create catch-all match use `else` clause",
                       "Replace `_` with `else` here"
        )

      let next = makeExpected(head)
      if toplevel.isNil:
        toplevel = next
      else:
        if next.kind != toplevel.kind:
          raiseCodeError(head, "#[ IMPLEMENT:ERRMSG ]#")


  # For each branch, update toplevel structure
  let path: Path = @[]

  for head in cs.heads:
    # echov head
    toplevel.updateExpected(head, path)
    # echov toplevel


  # Generate input tuple for expression
  let inputExpr = toplevel.makeInput(path)

  # Generate matcher expressions
  let matchcase = n.makeMatch(path, toplevel)

  # dieHere()


  let head = cs.expr
  result = quote do:
    block:
      let expr {.inject.} = `head`
      let input {.inject.} = `inputExpr`
      ifHaxComp:
        echo typeof(input)

      `matchcase`

  haxThis result.toStrLit()
  # dieHereMacro()
  assert inputExpr != nil
